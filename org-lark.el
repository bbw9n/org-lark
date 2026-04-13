;;; org-lark.el --- Export Lark docs to Org -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: bbw9n <bbw9nio@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: outlines, hypermedia, tools
;; URL: https://github.com/bbw9n/org-lark

;;; Commentary:

;; org-lark fetches Lark/Feishu document Markdown with lark-cli, normalizes
;; Lark-specific custom blocks, and converts the remaining Markdown to Org
;; with Pandoc.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-util)

(defgroup org-lark nil
  "Export Lark documents to Org."
  :group 'org
  :prefix "org-lark-")

(defcustom org-lark-cli-program "lark-cli"
  "Program name or absolute path for lark-cli."
  :type 'string)

(defcustom org-lark-pandoc-program "pandoc"
  "Program name or absolute path for Pandoc."
  :type 'string)

(defcustom org-lark-identity "user"
  "Identity passed to lark-cli with --as."
  :type '(choice (const "user") (const "bot") string))

(defcustom org-lark-page-size 50
  "Page size for paginated document fetching."
  :type 'integer)

(defcustom org-lark-download-media t
  "Whether to download image, file, and whiteboard assets."
  :type 'boolean)

(defcustom org-lark-assets-directory-name "assets"
  "Directory name, relative to the Org output file, for downloaded assets."
  :type 'string)

(defcustom org-lark-overwrite nil
  "Whether noninteractive exports may overwrite existing files."
  :type 'boolean)

(defcustom org-lark-table-span-strategy 'special-block
  "How to export Lark tables with rowspan or colspan attributes.

The value `special-block' preserves the original table as a
`lark_table' special block.  The value `lossy' emits an Org table
with explicit span markers in affected cells."
  :type '(choice (const :tag "Preserve as special block" special-block)
          (const :tag "Lossy native Org table" lossy)))

(defcustom org-lark-unresolved-mention-strategy 'link
  "How to represent mentions whose display names cannot be resolved."
  :type '(choice (const :tag "Custom Org link" link)
          (const :tag "Plain short id" short-id)))

(defcustom org-lark-preserve-inline-html nil
  "Whether to preserve inline styling as Org raw HTML snippets where possible."
  :type 'boolean)

(defconst org-lark--known-tags
  '("quote-container" "quote" "callout" "equation" "lark-table" "lark-tr"
    "lark-td" "grid" "column" "image" "whiteboard" "view" "file"
    "mention-doc" "mention-user" "chat-card" "link-preview" "iframe" "sheet"
    "add-ons" "agenda" "agenda-item" "agenda-item-title"
    "agenda-item-content" "source-synced" "reference-synced" "okr"
    "okr-objective" "task" "text")
  "Lark tags that org-lark intentionally parses.")

(defvar org-lark--placeholder-prefix "ORGLARK_PLACEHOLDER_")

(cl-defstruct org-lark--context
  output-file asset-directory placeholders notices)

;;; User commands

;;;###autoload
(defun org-lark-export (doc output-file)
  "Export Lark DOC URL or token to OUTPUT-FILE in Org format."
  (interactive
   (let* ((doc (read-string "Lark doc URL or token: "))
          (default-name (concat (file-name-as-directory default-directory)
                                "lark-export.org"))
          (output (read-file-name "Write Org file: " nil default-name nil
                                  (file-name-nondirectory default-name))))
     (list doc output)))
  (when (and (file-exists-p output-file)
             (not org-lark-overwrite)
             (not (called-interactively-p 'interactive)))
    (user-error "Refusing to overwrite %s" output-file))
  (when (and (called-interactively-p 'interactive)
             (file-exists-p output-file)
             (not org-lark-overwrite)
             (not (y-or-n-p (format "Overwrite %s? " output-file))))
    (user-error "Export cancelled"))
  (let* ((fetched (org-lark-fetch doc))
         (markdown (alist-get 'markdown fetched))
         (title (alist-get 'title fetched))
         (doc-id (alist-get 'doc_id fetched))
         (ctx (make-org-lark--context
               :output-file output-file
               :asset-directory
               (expand-file-name org-lark-assets-directory-name
                                 (file-name-directory
                                  (expand-file-name output-file)))
               :placeholders nil
               :notices (alist-get 'notices fetched)))
         (org (org-lark-convert-markdown markdown ctx))
         (org (org-lark-finalize-org org title doc doc-id ctx)))
    (make-directory (file-name-directory (expand-file-name output-file)) t)
    (with-temp-file output-file
      (insert org))
    (message "Exported %s" output-file)
    output-file))

;;;###autoload
(defun org-lark-export-url-at-point (output-file)
  "Export the Lark URL or token at point to OUTPUT-FILE."
  (interactive
   (list (read-file-name "Write Org file: " nil nil nil "lark-export.org")))
  (let ((doc (or (thing-at-point 'url t)
                 (thing-at-point 'symbol t))))
    (unless doc
      (user-error "No URL or token at point"))
    (org-lark-export doc output-file)))

;;; Fetching

(defun org-lark-fetch (doc)
  "Fetch DOC with lark-cli and return an alist.

The returned alist contains at least `markdown', `title', `doc_id',
and `notices'."
  (let ((offset 0)
        (markdowns nil)
        title doc-id notices has-more)
    (while (progn
             (let* ((json (org-lark--run-json
                           org-lark-cli-program
                           "docs" "+fetch" "--as" org-lark-identity
                           "--doc" doc "--format" "json"
                           "--offset" (number-to-string offset)
                           "--limit" (number-to-string org-lark-page-size)))
                    (ok (alist-get 'ok json))
                    (data (alist-get 'data json))
                    (notice (alist-get '_notice json)))
               (unless ok
                 (user-error "lark-cli fetch failed: %s"
                             (org-lark--json-encode
                              (or (alist-get 'error json) json))))
               (when notice
                 (push notice notices))
               (setq title (or title (alist-get 'title data)))
               (setq doc-id (or doc-id (alist-get 'doc_id data)))
               (push (or (alist-get 'markdown data) "") markdowns)
               (setq has-more (alist-get 'has_more data))
               (setq offset (+ offset org-lark-page-size))
               has-more)))
    `((markdown . ,(string-join (nreverse markdowns) "\n"))
      (title . ,title)
      (doc_id . ,doc-id)
      (notices . ,(nreverse notices)))))

;;; Conversion pipeline

(defun org-lark-convert-markdown (markdown ctx)
  "Convert Lark-flavored MARKDOWN to Org using CTX."
  (let* ((protected (org-lark--protect-fenced-code markdown ctx))
         (normalized (org-lark-normalize-lark-components protected ctx))
         (org (org-lark--run-pandoc normalized)))
    (setq org (org-lark--restore-placeholders org ctx))
    (setq org (org-lark--restore-deep-headings org))
    (setq org (org-lark--cleanup-org org))
    org))

(defun org-lark-normalize-lark-components (markdown ctx)
  "Normalize Lark custom components in MARKDOWN using CTX."
  (let ((text markdown))
    (setq text (org-lark--normalize-unsupported-comments text))
    (setq text (org-lark--replace-paired-tag
                text ctx "equation" #'org-lark--convert-equation))
    (setq text (org-lark--replace-paired-tag
                text ctx "quote-container" #'org-lark--convert-quote))
    (setq text (org-lark--replace-paired-tag
                text ctx "quote" #'org-lark--convert-quote))
    (setq text (org-lark--replace-paired-tag
                text ctx "callout" #'org-lark--convert-callout))
    (setq text (org-lark--replace-paired-tag
                text ctx "lark-table" #'org-lark--convert-table))
    (setq text (org-lark--replace-paired-tag
                text ctx "grid" #'org-lark--convert-grid))
    (setq text (org-lark--replace-paired-tag
                text ctx "agenda" #'org-lark--convert-agenda))
    (setq text (org-lark--replace-paired-tag
                text ctx "source-synced" #'org-lark--convert-source-synced))
    (setq text (org-lark--replace-paired-tag
                text ctx "reference-synced" #'org-lark--convert-reference-synced))
    (setq text (org-lark--replace-paired-tag
                text ctx "okr" #'org-lark--convert-okr))
    (setq text (org-lark--replace-paired-tag
                text ctx "view" #'org-lark--convert-view))
    (setq text (org-lark--replace-paired-tag
                text nil "text" #'org-lark--convert-inline-text))
    (setq text (org-lark--replace-paired-tag
                text nil "mention-doc" #'org-lark--convert-mention-doc))
    (setq text (org-lark--replace-self-closing-tags
                text ctx #'org-lark--convert-self-closing-tag))
    text))

;;; Paired block conversion

(defun org-lark--convert-equation (_attrs body _ctx)
  (concat "\\[\n" (string-trim body) "\n\\]\n"))

(defun org-lark--convert-quote (_attrs body ctx)
  (concat "#+begin_quote\n"
          (org-lark--convert-nested-markdown body ctx)
          "\n#+end_quote\n"))

(defun org-lark--convert-callout (attrs body ctx)
  (concat (org-lark--attr-line attrs)
          "#+begin_lark_callout\n"
          (org-lark--convert-nested-markdown body ctx)
          "\n#+end_lark_callout\n"))

(defun org-lark--convert-grid (attrs body ctx)
  (let ((inner (org-lark--replace-paired-tag
                body ctx "column" #'org-lark--convert-column)))
    (concat (org-lark--attr-line attrs)
            "#+begin_lark_grid\n"
            (string-trim inner)
            "\n#+end_lark_grid\n")))

(defun org-lark--convert-column (attrs body ctx)
  (concat (org-lark--attr-line attrs)
          "#+begin_lark_column\n"
          (org-lark--convert-nested-markdown body ctx)
          "\n#+end_lark_column\n"))

(defun org-lark--convert-source-synced (attrs body ctx)
  (concat (org-lark--attr-line attrs)
          "#+begin_lark_source_synced\n"
          (org-lark--convert-nested-markdown body ctx)
          "\n#+end_lark_source_synced\n"))

(defun org-lark--convert-reference-synced (attrs body ctx)
  (concat (org-lark--attr-line attrs)
          "#+begin_lark_reference_synced\n"
          (org-lark--convert-nested-markdown body ctx)
          "\n#+end_lark_reference_synced\n"))

(defun org-lark--convert-view (attrs body ctx)
  (concat (org-lark--attr-line attrs)
          "#+begin_lark_view\n"
          (org-lark--convert-nested-markdown body ctx)
          "\n#+end_lark_view\n"))

(defun org-lark--convert-agenda (_attrs body _ctx)
  (let ((items nil))
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (while (re-search-forward
              "<agenda-item\\([^>]*\\)>\\(\\(?:.\\|\n\\)*?\\)</agenda-item>"
              nil t)
        (let* ((item-body (match-string 2))
               (title (org-lark--first-paired-body
                       "agenda-item-title" item-body))
               (content (org-lark--first-paired-body
                         "agenda-item-content" item-body)))
          (push (concat "** " (string-trim (or title "Agenda item")) "\n"
                        (string-trim (or content "")) "\n")
                items))))
    (concat "* Lark agenda\n" (string-join (nreverse items) "\n"))))

(defun org-lark--convert-okr (attrs body _ctx)
  (let* ((parsed (org-lark--parse-attrs attrs))
         (period (or (alist-get "period-name-zh" parsed nil nil #'string=)
                     (alist-get "period-name-en" parsed nil nil #'string=)
                     "OKR"))
         (header (concat "* OKR " period "\n"
                         ":PROPERTIES:\n"
                         (org-lark--properties-from-attrs attrs)
                         ":END:\n"))
         (objectives nil))
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (while (re-search-forward
              "<okr-objective\\([^>]*\\)>\\(\\(?:.\\|\n\\)*?\\)</okr-objective>"
              nil t)
        (let ((obj-attrs (match-string 1))
              (obj-body (string-trim (match-string 2))))
          (push (concat "** " obj-body "\n"
                        ":PROPERTIES:\n"
                        (org-lark--properties-from-attrs obj-attrs)
                        ":END:\n")
                objectives))))
    (concat header (string-join (nreverse objectives) "\n"))))

(defun org-lark--convert-table (attrs body _ctx)
  (if (and (not (string-match-p "\\_<\\(?:rowspan\\|colspan\\)=" body))
           (not (eq org-lark-table-span-strategy 'special-block)))
      (org-lark--table-to-org attrs body t)
    (if (and (not (string-match-p "\\_<\\(?:rowspan\\|colspan\\)=" body))
             (eq org-lark-table-span-strategy 'special-block))
        (org-lark--table-to-org attrs body nil)
      (pcase org-lark-table-span-strategy
        ('lossy (org-lark--table-to-org attrs body t))
        (_ (concat (org-lark--attr-line attrs)
                   "#+begin_lark_table\n"
                   (string-trim body)
                   "\n#+end_lark_table\n"))))))

(defun org-lark--table-to-org (attrs body lossy)
  (let* ((parsed (org-lark--parse-attrs attrs))
         (header-row (string= (alist-get "header-row" parsed nil nil #'string=)
                              "true"))
         (rows nil))
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (while (re-search-forward
              "<lark-tr\\([^>]*\\)>\\(\\(?:.\\|\n\\)*?\\)</lark-tr>"
              nil t)
        (let ((row-body (match-string 2))
              (cells nil))
          (with-temp-buffer
            (insert row-body)
            (goto-char (point-min))
            (while (re-search-forward
                    "<lark-td\\([^>]*\\)>\\(\\(?:.\\|\n\\)*?\\)</lark-td>"
                    nil t)
              (let* ((cell-attrs (match-string 1))
                     (cell (string-trim (match-string 2))))
                (when (and lossy
                           (string-match-p "\\_<\\(?:rowspan\\|colspan\\)="
                                           cell-attrs))
                  (setq cell (concat "[" (string-trim cell-attrs) "] " cell)))
                (setq cell (replace-regexp-in-string "[\n\r]+[[:blank:]]*" " " cell))
                (push (org-lark--escape-org-table-cell cell) cells))))
          (push (nreverse cells) rows))))
    (let ((lines nil)
          (row-index 0))
      (dolist (row (nreverse rows))
        (setq row-index (1+ row-index))
        (push (concat "| " (string-join row " | ") " |") lines)
        (when (and header-row (= row-index 1))
          (push "|-" lines)))
      (concat (string-join (nreverse lines) "\n") "\n"))))

(defun org-lark--convert-inline-text (attrs body _ctx)
  (let ((parsed (org-lark--parse-attrs attrs)))
    (cond
     ((string= (alist-get "underline" parsed nil nil #'string=) "true")
      (concat "_" body "_"))
     ((and org-lark-preserve-inline-html
           (alist-get "bgcolor" parsed nil nil #'string=))
      (format "@@html:<mark style=\"background-color:%s\">%s</mark>@@"
              (alist-get "bgcolor" parsed nil nil #'string=)
              body))
     (t body))))

(defun org-lark--convert-mention-doc (attrs body _ctx)
  (let* ((parsed (org-lark--parse-attrs attrs))
         (token (alist-get "token" parsed nil nil #'string=))
         (type (alist-get "type" parsed nil nil #'string=))
         (label (if (string-empty-p (string-trim body))
                    "Lark doc"
                  (string-trim body))))
    (if token
        (format "[[lark-doc:%s][%s]] # lark-doc-type: %s"
                token label (or type "unknown"))
      label)))

;;; Self-closing conversion

(defun org-lark--convert-self-closing-tag (tag attrs ctx)
  (unless (member tag org-lark--known-tags)
    (throw 'org-lark-unknown nil))
  (pcase tag
    ("image" (org-lark--convert-image attrs ctx))
    ("whiteboard" (org-lark--convert-whiteboard attrs ctx))
    ("file" (org-lark--convert-file attrs ctx))
    ("mention-user" (org-lark--convert-mention-user attrs))
    ("chat-card" (org-lark--convert-chat-card attrs))
    ("link-preview" (org-lark--convert-link-preview attrs))
    ("iframe" (org-lark--convert-iframe attrs))
    ("sheet" (org-lark--convert-sheet attrs))
    ("add-ons" (org-lark--convert-addon attrs))
    ("task" (org-lark--convert-task attrs))
    (_ nil)))

(defun org-lark--convert-image (attrs ctx)
  (let* ((parsed (org-lark--parse-attrs attrs))
         (token (alist-get "token" parsed nil nil #'string=))
         (path (org-lark--media-path token nil ctx)))
    (concat (org-lark--attr-line attrs)
            (if path
                (format "[[file:%s]]\n" (org-lark--relative-output-path path ctx))
              (format "# Lark image token: %s\n" (or token ""))))))

(defun org-lark--convert-whiteboard (attrs ctx)
  (let* ((parsed (org-lark--parse-attrs attrs))
         (token (alist-get "token" parsed nil nil #'string=))
         (path (org-lark--media-path token "whiteboard" ctx)))
    (concat "#+caption: Lark whiteboard\n"
            (org-lark--attr-line attrs)
            (if path
                (format "[[file:%s]]\n" (org-lark--relative-output-path path ctx))
              (format "# Lark whiteboard token: %s\n" (or token ""))))))

(defun org-lark--convert-file (attrs ctx)
  (let* ((parsed (org-lark--parse-attrs attrs))
         (token (alist-get "token" parsed nil nil #'string=))
         (name (or (alist-get "name" parsed nil nil #'string=) "Lark file"))
         (path (org-lark--media-path token nil ctx)))
    (if path
        (format "[[file:%s][%s]]\n"
                (org-lark--relative-output-path path ctx)
                name)
      (format "# Lark file token: %s name: %s\n" (or token "") name))))

(defun org-lark--convert-mention-user (attrs)
  (let* ((parsed (org-lark--parse-attrs attrs))
         (id (alist-get "id" parsed nil nil #'string=))
         (short (and id (substring id 0 (min 10 (length id))))))
    (pcase org-lark-unresolved-mention-strategy
      ('short-id (format "@%s" (or short "user")))
      (_ (format "[[lark-user:%s][@%s]]" (or id "") (or short "user"))))))

(defun org-lark--convert-chat-card (attrs)
  (concat (org-lark--attr-line attrs)
          "#+begin_lark_chat_card\n"
          "#+end_lark_chat_card\n"))

(defun org-lark--convert-link-preview (attrs)
  (let* ((parsed (org-lark--parse-attrs attrs))
         (url (alist-get "url" parsed nil nil #'string=)))
    (if (and url (not (string-empty-p url)))
        (format "[[%s][Lark link preview]]\n" (org-lark--decode-url url))
      "# Empty Lark link preview\n")))

(defun org-lark--convert-iframe (attrs)
  (let* ((parsed (org-lark--parse-attrs attrs))
         (url (alist-get "url" parsed nil nil #'string=)))
    (concat "#+caption: Lark iframe\n"
            (org-lark--attr-line attrs)
            (if url
                (format "[[%s][Lark iframe]]\n" (org-lark--decode-url url))
              "# Lark iframe without URL\n"))))

(defun org-lark--convert-sheet (attrs)
  (let* ((parsed (org-lark--parse-attrs attrs))
         (token (alist-get "token" parsed nil nil #'string=)))
    (format "[[lark-sheet:%s][Lark sheet]]\n" (or token ""))))

(defun org-lark--convert-addon (attrs)
  (concat (org-lark--attr-line attrs)
          "#+begin_lark_addon\n"
          (string-trim attrs)
          "\n#+end_lark_addon\n"))

(defun org-lark--convert-task (attrs)
  (let* ((parsed (org-lark--parse-attrs attrs))
         (id (alist-get "task-id" parsed nil nil #'string=)))
    (format "[[lark-task:%s][Lark task]]\n" (or id ""))))

;;; Media

(defun org-lark--media-path (token type ctx)
  "Download TOKEN media of optional TYPE with CTX, returning a local path."
  (when (and org-lark-download-media token ctx)
    (make-directory (org-lark--context-asset-directory ctx) t)
    (let* ((base (expand-file-name
                  (concat (org-lark--safe-file-name token)
                          (when (string= type "whiteboard") "-whiteboard"))
                  (org-lark--context-asset-directory ctx)))
           (args (append (list "docs" "+media-download"
                               "--as" org-lark-identity
                               "--token" token
                               "--output" base)
                         (when type (list "--type" type))))
           (json (apply #'org-lark--run-json org-lark-cli-program args)))
      (unless (alist-get 'ok json)
        (user-error "lark-cli media download failed: %s"
                    (org-lark--json-encode
                     (or (alist-get 'error json) json))))
      (or (alist-get 'saved_path (alist-get 'data json))
          (alist-get 'output (alist-get 'data json))
          base))))

;;; Placeholder and parser helpers

(defun org-lark--protect-fenced-code (text ctx)
  "Replace fenced code blocks in TEXT with placeholders stored in CTX."
  (org-lark--replace-regexp text
                            "```[^\n]*\n\\(?:.\\|\n\\)*?\n```[ \t]*"
                            (lambda (match)
                              (org-lark--store-placeholder
                               (org-lark--fenced-code-to-org match)
                               ctx))))

(defun org-lark--fenced-code-to-org (block)
  "Convert a Markdown fenced code BLOCK to an Org source/example block."
  (let* ((lines (split-string block "\n"))
         (opening (or (car lines) "```"))
         (closing-index (max 1 (1- (length lines))))
         (body-lines (cl-subseq lines 1 closing-index))
         (body (string-join body-lines "\n"))
         (lang (when (string-match "^```\\([[:alnum:]_+-]+\\)?" opening)
                 (match-string 1 opening))))
    (if (and lang (not (string-empty-p lang)))
        (format "#+begin_src %s\n%s\n#+end_src\n" lang body)
      (format "#+begin_example\n%s\n#+end_example\n" body))))

(defun org-lark--store-placeholder (value ctx)
  (let* ((n (length (org-lark--context-placeholders ctx)))
         (key (format "%s%d" org-lark--placeholder-prefix n)))
    (setf (org-lark--context-placeholders ctx)
          (append (org-lark--context-placeholders ctx)
                  (list (cons key value))))
    key))

(defun org-lark--restore-placeholders (text ctx)
  (let ((result text))
    (dolist (entry (reverse (org-lark--context-placeholders ctx)) result)
      (setq result (replace-regexp-in-string
                    (regexp-quote (car entry))
                    (lambda (_match) (cdr entry))
                    result t t)))))

(defun org-lark--replace-paired-tag (text ctx tag converter)
  "Replace paired TAG in TEXT using CONVERTER.

CONVERTER is called with ATTRS, BODY, and CTX.  When CTX is
non-nil, converted content is protected as a placeholder."
  (let ((regexp (format "<%s\\([^>]*\\)>\\(\\(?:.\\|\n\\)*?\\)</%s>"
                        (regexp-quote tag) (regexp-quote tag))))
    (org-lark--replace-regexp
     text regexp
     (lambda (_match)
       (let ((converted (funcall converter
                                 (match-string 1)
                                 (match-string 2)
                                 ctx)))
         (if ctx
             (org-lark--store-placeholder converted ctx)
           converted))))))

(defun org-lark--convert-nested-markdown (markdown ctx)
  "Convert nested MARKDOWN to Org while reusing CTX placeholders."
  (if (not ctx)
      (string-trim markdown)
    (let* ((protected (org-lark--protect-fenced-code markdown ctx))
           (normalized (org-lark-normalize-lark-components protected ctx))
           (org (org-lark--run-pandoc normalized)))
      (string-trim org))))

(defun org-lark--replace-self-closing-tags (text ctx converter)
  (org-lark--replace-regexp
   text "<\\([A-Za-z][A-Za-z0-9-]*\\)\\([^>\n]*\\)/>"
   (lambda (match)
     (let ((tag (match-string 1))
           (attrs (match-string 2)))
       (catch 'org-lark-unknown
         (or (let ((converted (funcall converter tag attrs ctx)))
               (and converted
                    (if ctx
                        (org-lark--store-placeholder converted ctx)
                      converted)))
             match))))))

(defun org-lark--replace-regexp (text regexp replacer)
  "Return TEXT with REGEXP matches replaced by REPLACER."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let ((beg (match-beginning 0))
            (end (match-end 0))
            (replacement (funcall replacer (match-string 0))))
        (goto-char beg)
        (delete-region beg end)
        (insert replacement)))
    (buffer-string)))

(defun org-lark--normalize-unsupported-comments (text)
  (org-lark--replace-regexp
   text "<!--[ \t]*Unsupported block type:[ \t]*\\([^ \t\n-]+\\)[ \t]*-->"
   (lambda (_match)
     (format "# Unsupported Lark block type: %s" (match-string 1)))))

(defun org-lark--restore-deep-headings (text)
  (org-lark--replace-regexp
   text "^​?\\(#\\{7,\\}\\)[ \t]+\\(.+\\)$"
   (lambda (_match)
     (concat (make-string (length (match-string 1)) ?*)
             " "
             (match-string 2)))))

;;; Process helpers

(defun org-lark--run-json (program &rest args)
  "Run PROGRAM with ARGS and parse stdout as JSON."
  (let ((output (apply #'org-lark--run-program program args)))
    (json-parse-string output :object-type 'alist :array-type 'list)))

(defun org-lark--run-pandoc (markdown)
  "Convert MARKDOWN to Org with Pandoc."
  (org-lark--run-program-with-stdin org-lark-pandoc-program
                                    markdown
                                    "-f" "gfm" "-t" "org" "--wrap=none"))

(defun org-lark--run-program (program &rest args)
  "Run PROGRAM with ARGS and return stdout or signal an error."
  (with-temp-buffer
    (let ((status (apply #'process-file program nil (current-buffer) nil args)))
      (unless (zerop status)
        (user-error "%s failed with exit %s: %s"
                    program status (string-trim (buffer-string))))
      (buffer-string))))

(defun org-lark--run-program-with-stdin (program stdin &rest args)
  "Run PROGRAM with STDIN and ARGS, returning stdout."
  (let ((input-file (make-temp-file "org-lark-stdin-")))
    (unwind-protect
        (progn
          (with-temp-file input-file
            (insert stdin))
          (with-temp-buffer
            (let ((status (apply #'process-file program input-file
                                 (current-buffer) nil args)))
              (unless (zerop status)
                (user-error "%s failed with exit %s: %s"
                            program status (string-trim (buffer-string))))
              (buffer-string))))
      (ignore-errors (delete-file input-file)))))

;;; Formatting helpers

(defun org-lark-finalize-org (org title source doc-id ctx)
  "Add metadata to ORG for TITLE, SOURCE, DOC-ID and CTX."
  (concat "#+title: " (or title "Lark export") "\n"
          (when doc-id (format "#+lark_doc_id: %s\n" doc-id))
          "#+lark_source: " source "\n"
          "#+created_by: org-lark\n"
          (org-lark--notice-lines ctx)
          "\n"
          (string-trim org)
          "\n"))

(defun org-lark--notice-lines (ctx)
  (let ((lines nil))
    (dolist (notice (org-lark--context-notices ctx))
      (let ((update (alist-get 'update notice)))
        (when update
          (push (format "# lark-cli update available: current %s latest %s"
                        (alist-get 'current update)
                        (alist-get 'latest update))
                lines))))
    (if lines
        (concat (string-join (nreverse lines) "\n") "\n")
      "")))

(defun org-lark--cleanup-org (org)
  (let ((text org))
    (setq text (replace-regexp-in-string "\n\\{3,\\}" "\n\n" text))
    (string-trim text)))

(defun org-lark--attr-line (attrs)
  (let ((pairs (org-lark--parse-attrs attrs)))
    (if (null pairs)
        ""
      (concat "#+attr_org: "
              (mapconcat
               (lambda (pair)
                 (format ":%s %s" (car pair) (cdr pair)))
               pairs
               " ")
              "\n"))))

(defun org-lark--properties-from-attrs (attrs)
  (mapconcat
   (lambda (pair)
     (format ":%s: %s\n"
             (upcase (replace-regexp-in-string "-" "_" (car pair)))
             (cdr pair)))
   (org-lark--parse-attrs attrs)
   ""))

(defun org-lark--parse-attrs (attrs)
  "Parse XML-ish ATTRS into an alist."
  (let ((pairs nil))
    (with-temp-buffer
      (insert (or attrs ""))
      (goto-char (point-min))
      (while (re-search-forward
              "\\([A-Za-z0-9_-]+\\)=\"\\([^\"]*\\)\""
              nil t)
        (push (cons (match-string 1) (org-lark--decode-url (match-string 2)))
              pairs)))
    (nreverse pairs)))

(defun org-lark--first-paired-body (tag text)
  (when (string-match
         (format "<%s\\([^>]*\\)>\\(\\(?:.\\|\n\\)*?\\)</%s>"
                 (regexp-quote tag) (regexp-quote tag))
         text)
    (match-string 2 text)))

(defun org-lark--escape-org-table-cell (cell)
  (replace-regexp-in-string "|" "\\vert{}" (string-trim cell) t t))

(defun org-lark--decode-url (value)
  (if value
      (decode-coding-string (url-unhex-string value) 'utf-8)
    value))

(defun org-lark--safe-file-name (value)
  (replace-regexp-in-string "[^A-Za-z0-9._-]" "_" (or value "asset")))

(defun org-lark--relative-output-path (path ctx)
  (file-relative-name path
                      (file-name-directory
                       (expand-file-name
                        (org-lark--context-output-file ctx)))))

(defun org-lark--json-encode (value)
  (if (fboundp 'json-serialize)
      (json-serialize value)
    (json-encode value)))

(provide 'org-lark)

;;; org-lark.el ends here
