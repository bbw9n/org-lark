;;; org-lark.el --- Export Lark docs to Org -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: bbw9n <bbw9nio@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: outlines, hypermedia, tools
;; URL: https://github.com/bbw9n/org-lark

;;; Commentary:

;; org-lark exports Lark/Feishu cloud documents to Org files.
;;
;; Pipeline:
;;   1.  Fetch Markdown via `lark-cli docs +fetch'.
;;   2.  Pre-process: protect code blocks, replace Lark custom tags
;;       with placeholders or standard Markdown.
;;   3.  Run Pandoc exactly once on the whole document.
;;   4.  Restore placeholders, clean up, prepend metadata.
;;
;; Subprocess calls use `make-process' with a deadline so Emacs never
;; freezes even when lark-cli or Pandoc stalls.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-util)

;;;; Customization ──────────────────────────────────────────────────

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

(defcustom org-lark-download-media t
  "Whether to download images, files and whiteboard thumbnails."
  :type 'boolean)

(defcustom org-lark-assets-directory "assets"
  "Sub-directory for downloaded assets, relative to the Org output file."
  :type 'string)

(defcustom org-lark-timeout 30
  "Seconds before a subprocess call is killed."
  :type 'integer)

(defcustom org-lark-overwrite nil
  "Whether to overwrite an existing output file without asking."
  :type 'boolean)

(defcustom org-lark-open-after-export t
  "Whether interactive exports should visit the generated Org file."
  :type 'boolean)

(defcustom org-lark-debug nil
  "When non-nil, log to the *org-lark-log* buffer.
Toggle interactively with `org-lark-toggle-debug'."
  :type 'boolean)

;;;; Logging ───────────────────────────────────────────────────────

(defconst org-lark--log-buffer-name "*org-lark-log*")

(defun org-lark--log (fmt &rest args)
  "Append a timestamped line to the *org-lark-log* buffer.
Only active when `org-lark-debug' is non-nil."
  (when org-lark-debug
    (let ((line (apply #'format (concat "[%s] " fmt "\n")
                       (format-time-string "%T.%3N") args))
          (buf (get-buffer-create org-lark--log-buffer-name)))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (at-end (= (point) (point-max))))
          (save-excursion
            (goto-char (point-max))
            (insert line))
          (when at-end (goto-char (point-max))))
        (unless (eq major-mode 'special-mode)
          (special-mode))))))

;;;###autoload
(defun org-lark-toggle-debug ()
  "Toggle `org-lark-debug' and show the log buffer when enabled."
  (interactive)
  (setq org-lark-debug (not org-lark-debug))
  (if org-lark-debug
      (progn
        (display-buffer (get-buffer-create org-lark--log-buffer-name))
        (org-lark--log "debug logging enabled")
        (message "org-lark: debug ON — see %s" org-lark--log-buffer-name))
    (message "org-lark: debug OFF")))

;;;; Internal state ─────────────────────────────────────────────────

(cl-defstruct org-lark--state
  "Mutable bag threaded through a single export."
  output-file asset-dir (placeholders nil) (counter 0)
  (media-done 0) (media-total 0))

(defconst org-lark--ph-prefix "ORGLARKPH"
  "Placeholder token prefix.  Unlikely to collide with document text.")

;;;; User commands ──────────────────────────────────────────────────

;;;###autoload
(defun org-lark-export (doc output-file)
  "Export Lark DOC (URL or document token) to OUTPUT-FILE in Org format."
  (interactive
   (let* ((doc (read-string "Lark doc URL or token: "))
          (default (concat default-directory "lark-export.org"))
          (out (read-file-name "Write Org file: " nil default nil
                               (file-name-nondirectory default))))
     (list doc out)))
  (when (and (file-exists-p output-file)
             (not org-lark-overwrite)
             (if (called-interactively-p 'interactive)
                 (not (y-or-n-p (format "Overwrite %s? " output-file)))
               t))
    (user-error "Refusing to overwrite %s" output-file))
  (org-lark--log "export doc=%s → %s" doc output-file)
  (org-lark--msg "fetching...")
  (let* ((fetched (org-lark-fetch doc))
         (st (make-org-lark--state
              :output-file (expand-file-name output-file)
              :asset-dir (expand-file-name
                          org-lark-assets-directory
                          (file-name-directory
                           (expand-file-name output-file))))))
    (org-lark--log "fetched \"%s\" (%d chars)"
                   (alist-get 'title fetched)
                   (length (alist-get 'markdown fetched)))
    (when org-lark-download-media
      (setf (org-lark--state-media-total st)
            (org-lark--count-media (alist-get 'markdown fetched))))
    (org-lark--msg "converting...")
    (let ((org (org-lark--pipeline (alist-get 'markdown fetched)
                                   fetched doc st)))
      (make-directory (file-name-directory (expand-file-name output-file)) t)
      (let ((output-file (expand-file-name output-file)))
        (with-temp-file output-file (insert org))
        (org-lark--log "wrote %s (%d chars)" output-file (length org))
        (when (and org-lark-open-after-export
                   (called-interactively-p 'interactive))
          (find-file output-file))
        (org-lark--msg "done → %s" (file-name-nondirectory output-file))
        output-file))))

;;;###autoload
(defun org-lark-export-url-at-point (output-file)
  "Export the Lark URL or token at point to OUTPUT-FILE."
  (interactive
   (list (read-file-name "Write Org file: " nil nil nil "lark-export.org")))
  (let ((doc (or (thing-at-point 'url t) (thing-at-point 'symbol t))))
    (unless doc (user-error "No URL or token at point"))
    (org-lark-export doc output-file)))

;;;; Fetch ──────────────────────────────────────────────────────────

(defun org-lark-fetch (doc)
  "Fetch DOC from lark-cli.
Return alist with keys `markdown', `title', `doc_id'."
  (let* ((json (org-lark--run-json
                org-lark-cli-program
                "docs" "+fetch" "--as" org-lark-identity
                "--doc" doc "--format" "json"))
         (data (alist-get 'data json)))
    (unless (alist-get 'ok json)
      (user-error "lark-cli: %s"
                  (json-encode (or (alist-get 'error json) json))))
    `((markdown . ,(or (alist-get 'markdown data) ""))
      (title    . ,(alist-get 'title data))
      (doc_id   . ,(alist-get 'doc_id data)))))

;;;; Conversion pipeline ───────────────────────────────────────────

(defun org-lark--pipeline (markdown fetched source st)
  "Full pipeline: Lark MARKDOWN to finished Org string.
FETCHED holds metadata, SOURCE is the original URL/token."
  (let ((t0 (float-time)))
    (org-lark--log "pipeline: %d chars input" (length markdown))
    (let* ((text (org-lark--protect-code-blocks markdown st))
           (_    (org-lark--log "  code blocks → %d placeholders"
                                (org-lark--state-counter st)))
           (text (org-lark--normalize-tags text st))
           (_    (org-lark--log "  tags normalized → %d placeholders, %d chars for pandoc"
                                (org-lark--state-counter st) (length text)))
           (org  (org-lark--pandoc text))
           (org  (org-lark--restore-placeholders org st))
           (org  (org-lark--fix-deep-headings org))
           (org  (replace-regexp-in-string "\n\\{3,\\}" "\n\n" (string-trim org))))
      (org-lark--log "pipeline: %.2fs total" (- (float-time) t0))
      (concat "#+title: " (or (alist-get 'title fetched) "Lark export") "\n"
              (let ((id (alist-get 'doc_id fetched)))
                (if id (format "#+lark_doc_id: %s\n" id) ""))
              "#+lark_source: " source "\n"
              "#+created_by: org-lark\n\n"
              org "\n"))))

;;;; Code-block protection ─────────────────────────────────────────

(defun org-lark--protect-code-blocks (text st)
  "Replace fenced code blocks in TEXT with placeholders."
  (org-lark--re-replace
   text "```\\([^\n]*\\)\n\\(\\(?:.\\|\n\\)*?\\)\n```[ \t]*"
   (lambda ()
     (let* ((info (match-string 1))
            (body (match-string 2))
            (lang (when (string-match "^[[:alnum:]_+-]+" info)
                    (match-string 0 info))))
       (org-lark--ph
        (if (and lang (not (string-empty-p lang)))
            (format "\n#+begin_src %s\n%s\n#+end_src\n" lang body)
          (format "\n#+begin_example\n%s\n#+end_example\n" body))
        st)))))

;;;; Tag normalization ─────────────────────────────────────────────

(defun org-lark--normalize-tags (text st)
  "Replace all Lark custom tags in TEXT with placeholders or Markdown."
  (dolist (tag '("equation" "quote-container" "quote" "callout"
                 "lark-table" "grid" "agenda" "source-synced"
                 "reference-synced" "okr" "view" "text" "mention-doc"))
    (let ((before (org-lark--state-counter st)))
      (setq text (org-lark--replace-paired
                  text tag
                  (lambda (attrs body)
                    (org-lark--dispatch-paired tag attrs body st))))
      (let ((n (- (org-lark--state-counter st) before)))
        (when (> n 0)
          (org-lark--log "  <%s> %d match(es)" tag n)))))
  (let ((before (org-lark--state-counter st)))
    (setq text (org-lark--replace-self-closing text st))
    (let ((n (- (org-lark--state-counter st) before)))
      (when (> n 0)
        (org-lark--log "  self-closing: %d match(es)" n))))
  (setq text (replace-regexp-in-string
              "<!--[ \t]*Unsupported block type:[ \t]*\\([^ \t\n-]+\\)[ \t]*-->"
              "# Unsupported Lark block: \\1" text))
  text)

(defun org-lark--dispatch-paired (tag attrs body st)
  "Route paired TAG to the right handler."
  (pcase tag
    ("equation"
     (org-lark--ph (concat "\n\\[\n" (string-trim body) "\n\\]\n") st))
    ((or "quote-container" "quote")
     (org-lark--wrap-block attrs body st "quote"))
    ("callout"
     (org-lark--wrap-block attrs body st "lark_callout"))
    ("lark-table"
     (org-lark--tag-table attrs body st))
    ("grid"
     (org-lark--tag-grid attrs body st))
    ("agenda"
     (org-lark--tag-agenda attrs body st))
    ("okr"
     (org-lark--tag-okr attrs body st))
    ((or "source-synced" "reference-synced" "view")
     (org-lark--wrap-block
      attrs body st
      (concat "lark_" (replace-regexp-in-string "-" "_" tag))))
    ("text"
     (let ((parsed (org-lark--parse-attrs attrs)))
       (if (string= (alist-get "underline" parsed nil nil #'string=) "true")
           (org-lark--ph (concat "_" body "_") st)
         body)))
    ("mention-doc"
     (let* ((parsed (org-lark--parse-attrs attrs))
            (token (alist-get "token" parsed nil nil #'string=))
            (type  (alist-get "type"  parsed nil nil #'string=))
            (label (if (string-empty-p (string-trim body))
                       "Lark doc" (string-trim body))))
       (org-lark--ph
        (if token
            (format "[[lark-doc:%s][%s]] # type: %s" token label (or type ""))
          label)
        st)))
    (_ body)))

;;; Block-wrapper (quote, callout, view, …)

(defun org-lark--wrap-block (attrs body st block-name)
  "Wrap BODY in #+begin_BLOCK-NAME / #+end_BLOCK-NAME.
Org markers become placeholders; inner Markdown stays for Pandoc."
  (let ((inner (org-lark--normalize-tags body st))
        (attr  (org-lark--attr-line attrs)))
    (concat (org-lark--ph (concat attr "#+begin_" block-name "\n") st)
            inner
            (org-lark--ph (concat "\n#+end_" block-name "\n") st))))

;;; Table

(defun org-lark--tag-table (attrs body st)
  "Convert <lark-table> to an Org table placeholder."
  (let* ((parsed (org-lark--parse-attrs attrs))
         (header (string= (alist-get "header-row" parsed nil nil #'string=) "true"))
         (rows nil))
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (while (re-search-forward
              "<lark-tr\\([^>]*\\)>\\(\\(?:.\\|\n\\)*?\\)</lark-tr>" nil t)
        (let ((row-body (match-string 2)) cells)
          (with-temp-buffer
            (insert row-body)
            (goto-char (point-min))
            (while (re-search-forward
                    "<lark-td\\([^>]*\\)>\\(\\(?:.\\|\n\\)*?\\)</lark-td>" nil t)
              (let ((cell (replace-regexp-in-string
                           "[\n\r]+[[:blank:]]*" " "
                           (string-trim (match-string 2)))))
                (push (replace-regexp-in-string "|" "\\vert{}" cell t t) cells))))
          (push (nreverse cells) rows))))
    (let ((lines nil) (i 0))
      (dolist (row (nreverse rows))
        (cl-incf i)
        (push (concat "| " (string-join row " | ") " |") lines)
        (when (and header (= i 1))
          (push "|-" lines)))
      (org-lark--ph (concat (string-join (nreverse lines) "\n") "\n") st))))

;;; Grid + column

(defun org-lark--tag-grid (attrs body st)
  "Convert <grid> with inner <column> tags."
  (let ((inner (org-lark--replace-paired
                body "column"
                (lambda (col-attrs col-body)
                  (org-lark--wrap-block col-attrs col-body st "lark_column")))))
    (concat (org-lark--ph (concat (org-lark--attr-line attrs)
                                  "#+begin_lark_grid\n")
                          st)
            (string-trim inner)
            (org-lark--ph "\n#+end_lark_grid\n" st))))

;;; Agenda

(defun org-lark--tag-agenda (_attrs body st)
  "Convert <agenda> to Org heading tree."
  (let (items)
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (while (re-search-forward
              "<agenda-item\\([^>]*\\)>\\(\\(?:.\\|\n\\)*?\\)</agenda-item>" nil t)
        (let* ((item (match-string 2))
               (title (org-lark--first-paired-body "agenda-item-title" item))
               (content (org-lark--first-paired-body "agenda-item-content" item)))
          (push (concat "** " (string-trim (or title "Agenda item")) "\n"
                        (string-trim (or content "")) "\n")
                items))))
    (org-lark--ph
     (concat "* Lark agenda\n" (string-join (nreverse items) "\n"))
     st)))

;;; OKR

(defun org-lark--tag-okr (attrs body st)
  "Convert <okr> to Org heading tree."
  (let* ((parsed (org-lark--parse-attrs attrs))
         (period (or (alist-get "period-name-zh" parsed nil nil #'string=)
                     (alist-get "period-name-en" parsed nil nil #'string=)
                     "OKR"))
         (header (concat "* OKR " period "\n:PROPERTIES:\n"
                         (org-lark--props-from-attrs attrs) ":END:\n"))
         objectives)
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (while (re-search-forward
              "<okr-objective\\([^>]*\\)>\\(\\(?:.\\|\n\\)*?\\)</okr-objective>"
              nil t)
        (push (concat "** " (string-trim (match-string 2)) "\n:PROPERTIES:\n"
                      (org-lark--props-from-attrs (match-string 1)) ":END:\n")
              objectives)))
    (org-lark--ph
     (concat header (string-join (nreverse objectives) "\n"))
     st)))

;;;; Self-closing tags ─────────────────────────────────────────────

(defun org-lark--replace-self-closing (text st)
  "Replace <tag .../> self-closing tags in TEXT."
  (org-lark--re-replace
   text "<\\([A-Za-z][A-Za-z0-9-]*\\)\\([^>\n]*\\)/>"
   (lambda ()
     (let* ((full  (match-string 0))
            (tag   (match-string 1))
            (attrs (match-string 2))
            (result (org-lark--dispatch-self-closing tag attrs st)))
       (or result full)))))

(defun org-lark--dispatch-self-closing (tag attrs st)
  "Handle a self-closing TAG.  Return replacement or nil to keep original."
  (pcase tag
    ("image"        (org-lark--sc-media attrs nil st))
    ("whiteboard"   (org-lark--sc-media attrs "whiteboard" st))
    ("file"         (org-lark--sc-file attrs st))
    ("mention-user" (org-lark--sc-mention-user attrs st))
    ("chat-card"    (org-lark--ph (concat (org-lark--attr-line attrs)
                                          "#+begin_lark_chat_card\n"
                                          "#+end_lark_chat_card\n") st))
    ("link-preview" (let* ((p (org-lark--parse-attrs attrs))
                           (url (alist-get "url" p nil nil #'string=)))
                      (when url
                        (org-lark--ph (format "[[%s][Lark link preview]]\n" url) st))))
    ("iframe"       (let* ((p (org-lark--parse-attrs attrs))
                           (url (alist-get "url" p nil nil #'string=)))
                      (org-lark--ph
                       (if url (format "[[%s][Lark iframe]]\n" url)
                         "# Lark iframe without URL\n")
                       st)))
    ("sheet"        (let ((tok (alist-get "token" (org-lark--parse-attrs attrs)
                                          nil nil #'string=)))
                      (org-lark--ph
                       (format "[[lark-sheet:%s][Lark sheet]]\n" (or tok "")) st)))
    ("add-ons"      (org-lark--ph (concat (org-lark--attr-line attrs)
                                          "#+begin_lark_addon\n"
                                          (string-trim attrs)
                                          "\n#+end_lark_addon\n") st))
    ("task"         (let ((id (alist-get "task-id" (org-lark--parse-attrs attrs)
                                         nil nil #'string=)))
                      (org-lark--ph
                       (format "[[lark-task:%s][Lark task]]\n" (or id "")) st)))
    (_ nil)))

(defun org-lark--sc-media (attrs type st)
  "Handle <image/> and <whiteboard/> tags."
  (let* ((parsed (org-lark--parse-attrs attrs))
         (token  (alist-get "token" parsed nil nil #'string=))
         (path   (org-lark--download-media token type st)))
    (org-lark--ph
     (concat (org-lark--attr-line attrs)
             (if path
                 (format "[[file:%s]]\n" (org-lark--relative-path path st))
               (format "# Lark %s token: %s\n"
                       (or type "image") (or token ""))))
     st)))

(defun org-lark--sc-file (attrs st)
  "Handle <file/> tag."
  (let* ((parsed (org-lark--parse-attrs attrs))
         (token  (alist-get "token" parsed nil nil #'string=))
         (name   (or (alist-get "name" parsed nil nil #'string=) "Lark file"))
         (path   (org-lark--download-media token nil st)))
    (org-lark--ph
     (if path
         (format "[[file:%s][%s]]\n" (org-lark--relative-path path st) name)
       (format "# Lark file token: %s name: %s\n" (or token "") name))
     st)))

(defun org-lark--sc-mention-user (attrs st)
  "Handle <mention-user/> tag."
  (let* ((parsed (org-lark--parse-attrs attrs))
         (id     (alist-get "id" parsed nil nil #'string=))
         (short  (and id (substring id 0 (min 10 (length id))))))
    (org-lark--ph
     (format "[[lark-user:%s][@%s]]" (or id "") (or short "user"))
     st)))

;;;; Media download ────────────────────────────────────────────────

(defun org-lark--count-media (markdown)
  "Count the number of media tags in MARKDOWN for progress reporting."
  (with-temp-buffer
    (insert markdown)
    (let ((n 0))
      (goto-char (point-min))
      (while (re-search-forward
              "<\\(?:image\\|whiteboard\\|file\\)[ \t\n]" nil t)
        (cl-incf n))
      n)))

(defun org-lark--download-media (token type st)
  "Download media TOKEN of TYPE.  Return local path or nil."
  (when (and org-lark-download-media token)
    (cl-incf (org-lark--state-media-done st))
    (let ((total (org-lark--state-media-total st))
          (done  (org-lark--state-media-done st)))
      (if (> total 0)
          (org-lark--msg "downloading media %d/%d..." done total)
        (org-lark--msg "downloading media %d..." done)))
    (org-lark--log "media token=%s type=%s" token (or type "auto"))
    (make-directory (org-lark--state-asset-dir st) t)
    (let* ((output-dir (file-name-directory (org-lark--state-output-file st)))
           (relative-output
            (file-name-as-directory
             (file-relative-name (org-lark--state-asset-dir st) output-dir)))
           (base-name (concat (org-lark--safe-filename token)
                              (when (string= type "whiteboard") "-wb")))
           (base (concat relative-output base-name))
           (args (append (list "docs" "+media-download"
                               "--as" org-lark-identity
                               "--token" token "--output" base)
                         (when type (list "--type" type))))
           (json (condition-case err
                     (let ((default-directory output-dir))
                       (apply #'org-lark--run-json org-lark-cli-program args))
                   (error (org-lark--log "media FAILED: %s" (error-message-string err))
                          nil))))
      (when json
        (let ((data (alist-get 'data json)))
          (if (alist-get 'ok json)
              (let* ((path (or (alist-get 'saved_path data)
                               (alist-get 'output data) base))
                     (path (if (file-name-absolute-p path)
                               path
                             (expand-file-name path output-dir))))
                (org-lark--log "  saved → %s" path)
                path)
            (org-lark--log "media error token=%s" token)
            nil))))))

;;;; Placeholders ──────────────────────────────────────────────────

(defun org-lark--ph (value st)
  "Store VALUE in ST, return a unique placeholder token."
  (let ((key (format "%s%d_"
                     org-lark--ph-prefix
                     (cl-incf (org-lark--state-counter st)))))
    (push (cons key value) (org-lark--state-placeholders st))
    key))

(defun org-lark--restore-placeholders (text st)
  "Replace every placeholder in TEXT with its stored value."
  (let ((result text))
    (dolist (entry (org-lark--state-placeholders st) result)
      (setq result (replace-regexp-in-string
                    (regexp-quote (car entry))
                    (lambda (_) (cdr entry))
                    result t t)))))

;;;; Regex replace helper ──────────────────────────────────────────

(defun org-lark--re-replace (text regexp func)
  "Return TEXT with REGEXP matches replaced by results of FUNC.
FUNC is called with match data set.  Must extract groups immediately
before doing anything that might clobber match data."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let* ((beg (match-beginning 0))
             (end (match-end 0))
             (rep (funcall func)))
        (when rep
          (delete-region beg end)
          (goto-char beg)
          (insert rep))))
    (buffer-string)))

(defun org-lark--replace-paired (text tag func)
  "Replace every <TAG ...>BODY</TAG> in TEXT.
FUNC is called with (ATTRS BODY) strings."
  (let ((re (format "<%s\\([^>]*\\)>\\(\\(?:.\\|\n\\)*?\\)</%s>"
                     (regexp-quote tag) (regexp-quote tag))))
    (org-lark--re-replace
     text re
     (lambda ()
       (let ((attrs (match-string 1))
             (body  (match-string 2)))
         (funcall func attrs body))))))

;;;; Subprocess helpers ────────────────────────────────────────────

(defun org-lark--msg (fmt &rest args)
  "Show a status message in the minibuffer and force a redisplay."
  (apply #'message (concat "org-lark: " fmt) args)
  (redisplay t))

(defun org-lark--run (program &rest args)
  "Run PROGRAM with ARGS.  Return stdout.
Uses `make-process' with a deadline so Emacs stays responsive."
  (let* ((bin  (file-name-nondirectory program))
         (cmd  (format "%s %s" bin (mapconcat #'shell-quote-argument args " ")))
         (stdout-buf (generate-new-buffer " *org-lark*"))
         (stderr-buf (generate-new-buffer " *org-lark-err*"))
         (t0 (float-time)))
    (org-lark--log "$ %s" cmd)
    (unwind-protect
        (let* ((proc (make-process
                      :name "org-lark"
                      :buffer stdout-buf
                      :stderr stderr-buf
                      :command (cons program args)
                      :connection-type 'pipe
                      :noquery t
                      :sentinel #'ignore))
               (deadline (+ (float-time) org-lark-timeout)))
          (while (process-live-p proc)
            (accept-process-output proc 1)
            (when (> (float-time) deadline)
              (delete-process proc)
              (org-lark--log "TIMEOUT %ds: %s" org-lark-timeout cmd)
              (user-error "org-lark: %s timed out (%ds)" bin org-lark-timeout)))
          (accept-process-output proc 0.1)
          (let ((exit-code (process-exit-status proc))
                (out-len (with-current-buffer stdout-buf (buffer-size)))
                (elapsed (- (float-time) t0)))
            (org-lark--log "  → %.1fs, exit %d, %d bytes" elapsed exit-code out-len)
            (unless (zerop exit-code)
              (let ((err (with-current-buffer stderr-buf
                           (string-trim (buffer-string)))))
                (org-lark--log "  FAILED: %s" err)
                (user-error "org-lark: %s failed (exit %d): %s" bin exit-code err)))
            (with-current-buffer stdout-buf (buffer-string))))
      (ignore-errors (kill-buffer stdout-buf))
      (ignore-errors (kill-buffer stderr-buf)))))

(defun org-lark--run-json (program &rest args)
  "Run PROGRAM with ARGS, parse stdout as JSON alist."
  (json-parse-string (apply #'org-lark--run program args)
                     :object-type 'alist :array-type 'list))

(defun org-lark--pandoc (markdown)
  "Convert MARKDOWN to Org with a single Pandoc call."
  (org-lark--msg "running pandoc...")
  (let ((in-file (make-temp-file "org-lark-" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file in-file (insert markdown))
          (org-lark--run org-lark-pandoc-program
                         "-f" "gfm" "-t" "org" "--wrap=none" in-file))
      (delete-file in-file t))))

;;;; Post-processing ───────────────────────────────────────────────

(defun org-lark--fix-deep-headings (text)
  "Convert Markdown-style ####### headings beyond level 6 to Org stars."
  (org-lark--re-replace
   text "^\\(?:\u200b\\)?\\(#\\{7,\\}\\)[ \t]+\\(.+\\)$"
   (lambda ()
     (concat (make-string (length (match-string 1)) ?*)
             " " (match-string 2)))))

;;;; Attribute / formatting helpers ────────────────────────────────

(defun org-lark--parse-attrs (attrs)
  "Parse XML-ish ATTRS string into an alist of (KEY . VALUE)."
  (let (pairs)
    (with-temp-buffer
      (insert (or attrs ""))
      (goto-char (point-min))
      (while (re-search-forward "\\([A-Za-z0-9_-]+\\)=\"\\([^\"]*\\)\"" nil t)
        (push (cons (match-string 1)
                    (decode-coding-string (url-unhex-string (match-string 2))
                                          'utf-8))
              pairs)))
    (nreverse pairs)))

(defun org-lark--attr-line (attrs)
  "Build #+attr_org: line from ATTRS string, or empty string."
  (let ((pairs (org-lark--parse-attrs attrs)))
    (if pairs
        (concat "#+attr_org: "
                (mapconcat (lambda (p) (format ":%s %s" (car p) (cdr p)))
                           pairs " ")
                "\n")
      "")))

(defun org-lark--props-from-attrs (attrs)
  "Build :PROPERTY: lines from ATTRS string."
  (mapconcat (lambda (p)
               (format ":%s: %s\n"
                       (upcase (replace-regexp-in-string "-" "_" (car p)))
                       (cdr p)))
             (org-lark--parse-attrs attrs) ""))

(defun org-lark--first-paired-body (tag text)
  "Return the body of the first <TAG>BODY</TAG> in TEXT, or nil."
  (when (string-match
         (format "<%s[^>]*>\\(\\(?:.\\|\n\\)*?\\)</%s>"
                 (regexp-quote tag) (regexp-quote tag))
         text)
    (match-string 1 text)))

(defun org-lark--safe-filename (s)
  "Sanitize S for use as a filename."
  (replace-regexp-in-string "[^A-Za-z0-9._-]" "_" (or s "asset")))

(defun org-lark--relative-path (path st)
  "Return PATH relative to the output file directory in ST."
  (file-relative-name path (file-name-directory (org-lark--state-output-file st))))

(provide 'org-lark)

;;; org-lark.el ends here
