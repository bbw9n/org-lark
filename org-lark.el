;;; org-lark.el --- Export Lark docs to Org -*- lexical-binding: t; -*-

;; Copyright (C) 2026  bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: outlines, hypermedia, tools
;; URL: https://github.com/bbw9n/org-lark

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

(defcustom org-lark-publish-default-parent nil
  "Default parent for newly-created Lark docs.
String of the form \"folder:TOKEN\" or \"wiki:SPACE/NODE\", or nil.
Overridden by a per-file =#+lark_parent:= keyword."
  :type '(choice (const nil) string))

(defcustom org-lark-publish-update-mode "overwrite"
  "Mode passed to =lark-cli docs +update= when re-publishing.
The default \"overwrite\" replaces the whole document body."
  :type '(choice (const "overwrite") (const "append")
                 (const "replace_all") string))

(defcustom org-lark-confirm-overwrite-remote t
  "When non-nil, prompt before overwriting an existing remote doc.
Skipped for non-interactive callers."
  :type 'boolean)

(defcustom org-lark-media-cache-file
  (locate-user-emacs-file "org-lark-media.eld")
  "Where to persist the media upload cache.
The cache maps (absolute-path . sha256) → uploaded Lark token so
unchanged assets reuse their token across publish runs."
  :type 'file)

;;;; Logging ───────────────────────────────────────────────────────

(defconst org-lark--log-buffer-name "*org-lark-log*")

(defun org-lark--log (fmt &rest args)
  "Append a timestamped line built from FMT and ARGS to the log buffer.
Only active when `org-lark-debug' is non-nil."
  (when org-lark-debug
    (let ((line (apply #'format (concat "[%s] " fmt "\n")
                       (format-time-string "%T.%3N") args))
          (buf (get-buffer-create org-lark--log-buffer-name)))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (at-end (eobp)))
          (save-excursion
            (goto-char (point-max))
            (insert line))
          (when at-end (goto-char (point-max))))
        (unless (derived-mode-p 'special-mode)
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
  (media-done 0) (media-total 0)
  ;; Deferred media downloads — populated by sc-media/sc-file when
  ;; `org-lark--defer-media' is non-nil.  Each entry is
  ;; (KEY . PLIST) where PLIST has :kind :token :type :attrs :name.
  ;; Resolved post-pandoc by the async export driver.
  (media-jobs nil))

(defconst org-lark--ph-prefix "ORGLARKPH"
  "Placeholder token prefix.  Unlikely to collide with document text.")

(defvar org-lark--defer-media nil
  "When non-nil, sc-media and sc-file queue downloads instead of running them.
The async export driver binds this so media downloads can be
fanned out in parallel after the sync parse phase completes.")

;;;; User commands ──────────────────────────────────────────────────

;;;###autoload
(defun org-lark-export-async (doc output-file callback)
  "Export Lark DOC to OUTPUT-FILE asynchronously.
Returns nil immediately.  CALLBACK is invoked with (ERR PATH) on
completion: ERR is nil and PATH is the absolute output file path
on success; on failure ERR is a descriptive string and PATH is nil.
The whole pipeline (fetch, pandoc, media downloads) runs through
async subprocesses so Emacs stays interactive throughout."
  (when (and (file-exists-p output-file)
             (not org-lark-overwrite))
    (user-error "Refusing to overwrite %s" output-file))
  (org-lark--log "export doc=%s → %s" doc output-file)
  (org-lark--msg "fetching...")
  (let* ((output-file (expand-file-name output-file))
         (st (make-org-lark--state
              :output-file output-file
              :asset-dir (expand-file-name
                          org-lark-assets-directory
                          (file-name-directory output-file)))))
    (org-lark-fetch-async
     doc
     (lambda (err fetched)
       (cond
        (err
         (org-lark--msg "fetch failed: %s" err)
         (funcall callback err nil))
        (t
         (org-lark--log "fetched \"%s\" (%d chars)"
                        (alist-get 'title fetched)
                        (length (alist-get 'markdown fetched)))
         (org-lark--msg "converting...")
         (org-lark--pipeline-async
          (alist-get 'markdown fetched) fetched doc st
          (lambda (err org-text)
            (cond
             (err
              (org-lark--msg "convert failed: %s" err)
              (funcall callback err nil))
             (t
              (condition-case write-err
                  (progn
                    (make-directory (file-name-directory output-file) t)
                    (with-temp-file output-file (insert org-text))
                    (org-lark--log "wrote %s (%d chars)"
                                   output-file (length org-text))
                    (org-lark--msg "done → %s"
                                   (file-name-nondirectory output-file))
                    (funcall callback nil output-file))
                (error
                 (let ((msg (error-message-string write-err)))
                   (org-lark--log "write failed: %s" msg)
                   (funcall callback msg nil))))))))))))
    nil))

;;;###autoload
(defun org-lark-export (doc output-file &optional callback)
  "Export Lark DOC (URL or document token) to OUTPUT-FILE in Org format.

When called interactively or with a CALLBACK argument, runs
asynchronously and returns nil immediately; the buffer stays
responsive during fetch, conversion and media downloads.  In the
interactive case, the file is opened on completion when
`org-lark-open-after-export' is non-nil.

When called programmatically without CALLBACK, blocks until the
export finishes and returns the absolute output path (matching
the pre-async API)."
  (interactive
   (let* ((doc (read-string "Lark doc URL or token: "))
          (default (concat default-directory "lark-export.org"))
          (out (read-file-name "Write Org file: " nil default nil
                               (file-name-nondirectory default))))
     (list doc out)))
  (when (and (file-exists-p output-file)
             (not org-lark-overwrite)
             (called-interactively-p 'interactive)
             (not (y-or-n-p (format "Overwrite %s? " output-file))))
    (user-error "Refusing to overwrite %s" output-file))
  (cond
   ;; Programmatic + callback supplied: fire-and-forget.
   (callback
    (org-lark-export-async doc output-file callback))
   ;; Interactive: async, status via message, optional auto-open.
   ((called-interactively-p 'interactive)
    (org-lark-export-async
     doc output-file
     (lambda (err path)
       (cond
        (err (message "org-lark: export failed: %s" err))
        (t (when org-lark-open-after-export
             (find-file path)))))))
   ;; Programmatic, no callback: block until done so existing
   ;; sync callers still receive the output path.
   (t
    (let ((done nil) (err nil) (result nil))
      (org-lark-export-async
       doc output-file
       (lambda (e p) (setq done t err e result p)))
      (while (not done) (accept-process-output nil 0.05))
      (when err (user-error "org-lark: %s" err))
      result))))

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
FETCHED holds metadata, SOURCE is the original URL/token.
ST is the mutable export state."
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
           (org  (org-lark--normalize-attr-keyword-lines org))
           (org  (org-lark--fix-deep-headings org))
           (org  (replace-regexp-in-string "\n\\{3,\\}" "\n\n" (string-trim org))))
      (org-lark--log "pipeline: %.2fs total" (- (float-time) t0))
      (concat "#+title: " (or (alist-get 'title fetched) "Lark export") "\n"
              (let ((id (alist-get 'doc_id fetched)))
                (if id (format "#+lark_doc_id: %s\n" id) ""))
              "#+lark_source: " source "\n"
              "#+created_by: org-lark\n\n"
              org "\n"))))

(defun org-lark--metadata-header (fetched source)
  "Build the #+title / #+lark_* preamble for FETCHED and SOURCE."
  (concat "#+title: " (or (alist-get 'title fetched) "Lark export") "\n"
          (let ((id (alist-get 'doc_id fetched)))
            (if id (format "#+lark_doc_id: %s\n" id) ""))
          "#+lark_source: " source "\n"
          "#+created_by: org-lark\n\n"))

(defun org-lark--pipeline-async (markdown fetched source st callback)
  "Async variant of `org-lark--pipeline'.
Defers media downloads (via the dynamic `org-lark--defer-media'
binding), runs pandoc and media downloads asynchronously, and
calls CALLBACK with (ERR ORG-STRING)."
  (let ((t0 (float-time)))
    (org-lark--log "pipeline-async: %d chars input" (length markdown))
    (let* ((org-lark--defer-media t)
           (text (org-lark--protect-code-blocks markdown st))
           (text (org-lark--normalize-tags text st)))
      (org-lark--log "  parse → %d placeholders, %d media jobs"
                     (org-lark--state-counter st)
                     (length (org-lark--state-media-jobs st)))
      (org-lark--pandoc-async
       text
       (lambda (err org-text)
         (cond
          (err (funcall callback err nil))
          (t
           (let* ((org-text (org-lark--restore-placeholders org-text st))
                  (org-text (org-lark--normalize-attr-keyword-lines org-text))
                  (org-text (org-lark--fix-deep-headings org-text)))
             (org-lark--download-all-media-async
              st
              (lambda (results)
                (let* ((final (org-lark--substitute-media-results
                               org-text st results))
                       (final (replace-regexp-in-string
                               "\n\\{3,\\}" "\n\n" (string-trim final))))
                  (org-lark--log "pipeline-async: %.2fs total"
                                 (- (float-time) t0))
                  (funcall callback nil
                           (concat (org-lark--metadata-header fetched source)
                                   final "\n"))))))))))
      nil)))

;;;; Code-block protection ─────────────────────────────────────────

(defun org-lark--protect-code-blocks (text st)
  "Replace fenced code blocks in TEXT with placeholders in ST."
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
  "Replace all Lark custom tags in TEXT with placeholders or Markdown.
ST is the mutable export state."
  (setq text (org-lark--flatten-nested text st))
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
  "Route paired TAG with ATTRS and BODY to the right handler.
ST is the mutable export state."
  (pcase tag
    ("equation"
     (org-lark--ph (concat "\n\\[\n" (string-trim body) "\n\\]\n") st))
    ;; Native Org block keeps native rendering; the `lark-block'
    ;; sentinel in #+attr_org disambiguates from a plain quote/example
    ;; on republish, even if `#+attr_org' ends up inlined inside a list.
    ((or "quote-container" "quote")
     (org-lark--wrap-block (org-lark--with-sentinel "quote-container" attrs)
                           body st "quote"))
    ("callout"
     (org-lark--wrap-block (org-lark--with-sentinel "callout" attrs)
                           body st "example"))
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
ATTRS are forwarded as #+attr_org.  ST holds placeholders.
Org markers become placeholders; inner Markdown stays for Pandoc."
  (let ((inner (org-lark--normalize-tags body st))
        (attr  (org-lark--attr-line attrs)))
    (concat (org-lark--ph (concat attr "#+begin_" block-name "\n") st)
            inner
            (org-lark--ph (concat "\n#+end_" block-name "\n") st))))

;;; Table

(defun org-lark--tag-table (attrs body st)
  "Convert <lark-table> with ATTRS and BODY to an Org table placeholder in ST."
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
    (let* ((rows (nreverse rows))
           ;; Compute max width per column for alignment
           (ncols (apply #'max 0 (mapcar #'length rows)))
           (widths (make-vector ncols 0))
           (_ (dolist (row rows)
                (cl-loop for cell in row for c from 0 do
                         (aset widths c (max (aref widths c)
                                             (string-width cell))))))
           (lines nil) (i 0))
      (dolist (row rows)
        (cl-incf i)
        (let ((padded
               (cl-loop for cell in row for c from 0
                        collect (let ((w (aref widths c)))
                                  (concat cell
                                          (make-string (- w (string-width cell))
                                                       ?\s))))))
          (push (concat "| " (string-join padded " | ") " |") lines))
        (when (and header (= i 1))
          (push (concat "|"
                        (mapconcat (lambda (w) (make-string (+ w 2) ?-))
                                   (append widths nil) "+")
                        "|")
                lines)))
      (org-lark--ph (concat (string-join (nreverse lines) "\n") "\n") st))))

;;; Grid + column

(defun org-lark--tag-grid (attrs body st)
  "Convert <grid> with ATTRS and BODY containing inner <column> tags.
ST is the mutable export state."
  (let ((inner (org-lark--replace-paired
                body "column"
                (lambda (col-attrs col-body)
                  (org-lark--wrap-block col-attrs col-body st "lark_column")))))
    (concat (org-lark--ph (concat (org-lark--attr-line attrs)
                                  "#+begin_lark_grid\n")
                          st)
            (string-trim inner)
            (org-lark--ph "\n#+end_lark_grid\n" st))))

;;; Nesting pre-pass (quote-in-table, grid-in-table, table-in-grid)

(defun org-lark--flatten-nested (text st)
  "Pre-pass: strip quotes in tables, flatten nested table/grid in TEXT to lists.
ST is the mutable export state."
  (setq text (org-lark--preprocess-tables text st))
  (setq text (org-lark--preprocess-grids text st))
  text)

(defun org-lark--preprocess-tables (text st)
  "Strip quote/callout wrappers in table cells of TEXT; flatten tables with grids.
ST is the mutable export state."
  (let ((re "<lark-table\\([^>]*\\)>\\(\\(?:.\\|\n\\)*?\\)</lark-table>"))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (let* ((beg (match-beginning 0))
               (end (match-end 0))
               (attrs (match-string 1))
               (body (match-string 2))
               (body (replace-regexp-in-string
                      "</?\\(?:quote-container\\|quote\\|callout\\)[^>]*>"
                      "" body)))
          (delete-region beg end)
          (goto-char beg)
          (if (string-match-p "<grid[ >]" body)
              (insert (org-lark--table-to-list attrs body st))
            (insert (format "<lark-table%s>%s</lark-table>" attrs body)))))
      (buffer-string))))

(defun org-lark--preprocess-grids (text st)
  "Flatten grids containing tables in TEXT to lists.
ST is the mutable export state."
  (let ((re "<grid\\([^>]*\\)>\\(\\(?:.\\|\n\\)*?\\)</grid>"))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (let* ((beg (match-beginning 0))
               (end (match-end 0))
               (attrs (match-string 1))
               (body (match-string 2)))
          (when (string-match-p "<lark-table[ >]" body)
            (delete-region beg end)
            (goto-char beg)
            (insert (org-lark--grid-to-list attrs body st)))))
      (buffer-string))))

(defun org-lark--extract-table-rows (body)
  "Parse table BODY into a list of rows, each a list of cell strings."
  (let (rows)
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
              (push (string-trim (match-string 2)) cells)))
          (push (nreverse cells) rows))))
    (nreverse rows)))

(defun org-lark--table-to-list (attrs body st)
  "Convert a <lark-table> with ATTRS and BODY to an Org list placeholder in ST."
  (let* ((parsed (org-lark--parse-attrs attrs))
         (header-p (string= (alist-get "header-row" parsed nil nil #'string=) "true"))
         (body (org-lark--inline-nested-grid body))
         (rows (org-lark--extract-table-rows body))
         (headers (when (and header-p rows) (pop rows)))
         (lines nil))
    (dolist (row rows)
      (if headers
          (let ((parts nil) (i 0))
            (dolist (cell row)
              (let ((h (or (nth i headers) (format "Col %d" (1+ i)))))
                (push (format "*%s*: %s" h cell) parts))
              (cl-incf i))
            (push (concat "- " (string-join (nreverse parts) " | ")) lines))
        (push (concat "- " (string-join row " | ")) lines)))
    (org-lark--ph (concat "\n" (string-join (nreverse lines) "\n") "\n") st)))

(defun org-lark--grid-to-list (_attrs body st)
  "Convert a <grid> with BODY containing tables to an Org list placeholder in ST."
  (let ((columns nil) (col-idx 0))
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (while (re-search-forward
              "<column\\([^>]*\\)>\\(\\(?:.\\|\n\\)*?\\)</column>" nil t)
        (cl-incf col-idx)
        (let* ((col-body (string-trim (match-string 2)))
               (col-body (org-lark--inline-nested-table col-body)))
          (push (format "- Column %d\n%s" col-idx
                        (replace-regexp-in-string "^" "  " col-body))
                columns))))
    (org-lark--ph
     (concat "\n" (string-join (nreverse columns) "\n") "\n") st)))

(defun org-lark--inline-nested-table (text)
  "Replace <lark-table> blocks in TEXT with simple list items."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((re "<lark-table\\([^>]*\\)>\\(\\(?:.\\|\n\\)*?\\)</lark-table>"))
      (while (re-search-forward re nil t)
        (let* ((beg (match-beginning 0))
               (end (match-end 0))
               (body (match-string 2))
               (rows (org-lark--extract-table-rows body))
               (list-text (mapconcat
                           (lambda (row)
                             (concat "- " (string-join row " | ")))
                           rows "\n")))
          (delete-region beg end)
          (goto-char beg)
          (insert list-text))))
    (buffer-string)))

(defun org-lark--inline-nested-grid (text)
  "Replace <grid> blocks in TEXT with column content joined by \" / \"."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((re "<grid\\([^>]*\\)>\\(\\(?:.\\|\n\\)*?\\)</grid>"))
      (while (re-search-forward re nil t)
        (let* ((beg (match-beginning 0))
               (end (match-end 0))
               (body (match-string 2))
               (columns nil))
          (with-temp-buffer
            (insert body)
            (goto-char (point-min))
            (while (re-search-forward
                    "<column\\([^>]*\\)>\\(\\(?:.\\|\n\\)*?\\)</column>" nil t)
              (push (string-trim (match-string 2)) columns)))
          (let ((content (string-join (nreverse columns) " / ")))
            (delete-region beg end)
            (goto-char beg)
            (insert content)))))
    (buffer-string)))

;;; Agenda

(defun org-lark--tag-agenda (_attrs body st)
  "Convert <agenda> BODY to Org heading tree placeholder in ST."
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
  "Convert <okr> with ATTRS and BODY to Org heading tree in ST."
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
  "Replace <tag .../> self-closing tags in TEXT using ST for placeholders."
  (org-lark--re-replace
   text "<\\([A-Za-z][A-Za-z0-9-]*\\)\\([^>\n]*\\)/>"
   (lambda ()
     (let* ((full  (match-string 0))
            (tag   (match-string 1))
            (attrs (match-string 2))
            (result (org-lark--dispatch-self-closing tag attrs st)))
       (or result full)))))

(defun org-lark--dispatch-self-closing (tag attrs st)
  "Handle a self-closing TAG with ATTRS.  Return replacement or nil.
ST is the mutable export state."
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
  "Handle <image/> and <whiteboard/> tags with ATTRS and TYPE in ST.
When `org-lark--defer-media' is non-nil and downloads are enabled,
queues the download into ST.media-jobs and returns a deferred
placeholder; the async driver substitutes it after fan-out."
  (let* ((parsed (org-lark--parse-attrs attrs))
         (token  (alist-get "token" parsed nil nil #'string=)))
    (cond
     ((and org-lark--defer-media org-lark-download-media token)
      (org-lark--ph-media
       (if (string= type "whiteboard") 'whiteboard 'image)
       token type attrs nil st))
     (t
      (let ((path (org-lark--download-media token type st)))
        (org-lark--ph
         (concat (org-lark--attr-line attrs)
                 (if path
                     (format "[[file:%s]]\n" (org-lark--relative-path path st))
                   (format "# Lark %s token: %s\n"
                           (or type "image") (or token ""))))
         st))))))

(defun org-lark--sc-file (attrs st)
  "Handle <file/> tag with ATTRS, storing placeholder in ST.
Honours `org-lark--defer-media' the same way as `org-lark--sc-media'."
  (let* ((parsed (org-lark--parse-attrs attrs))
         (token  (alist-get "token" parsed nil nil #'string=))
         (name   (or (alist-get "name" parsed nil nil #'string=) "Lark file")))
    (cond
     ((and org-lark--defer-media org-lark-download-media token)
      (org-lark--ph-media 'file token nil attrs name st))
     (t
      (let ((path (org-lark--download-media token nil st)))
        (org-lark--ph
         (if path
             (format "[[file:%s][%s]]\n" (org-lark--relative-path path st) name)
           (format "# Lark file token: %s name: %s\n" (or token "") name))
         st))))))

(defun org-lark--sc-mention-user (attrs st)
  "Handle <mention-user/> tag with ATTRS, storing placeholder in ST."
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
  "Download media TOKEN of TYPE using ST for paths.  Return local path or nil."
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
  ;; Terminator is a letter (`Z') rather than `_' so pandoc's GFM writer
  ;; doesn't escape it as italic-marker punctuation (e.g. `ORGLARKPH1_'
  ;; → `ORGLARKPH1\_'), which would silently break placeholder
  ;; restoration and drop every Lark block we wrapped.
  (let ((key (format "%s%dZ"
                     org-lark--ph-prefix
                     (cl-incf (org-lark--state-counter st)))))
    (push (cons key value) (org-lark--state-placeholders st))
    key))

(defun org-lark--ph-media (kind token type attrs name st)
  "Allocate a deferred-media placeholder in ST.
KIND is `image', `whiteboard', or `file'.  TOKEN/TYPE are forwarded
to lark-cli +media-download.  ATTRS is the original tag attrs string
(used to render #+attr_org on success).  NAME is the display label
for file links.  Returns a placeholder key string."
  (let ((key (format "%s%dZ"
                     org-lark--ph-prefix
                     (cl-incf (org-lark--state-counter st)))))
    (push (cons key (list :kind kind :token token :type type
                          :attrs attrs :name name))
          (org-lark--state-media-jobs st))
    key))

(defun org-lark--restore-placeholders (text st)
  "Replace every placeholder in TEXT with its stored value from ST.
ST may be either an `org-lark--state' (read path) or an
`org-lark--pubstate' (publish path)."
  (let ((result text)
        (entries (cond
                  ((org-lark--state-p st)
                   (org-lark--state-placeholders st))
                  ((org-lark--pubstate-p st)
                   (org-lark--pubstate-placeholders st))
                  (t (error "Unknown state type passed to restore: %S"
                            (type-of st))))))
    (dolist (entry entries result)
      (setq result (replace-regexp-in-string
                    (regexp-quote (car entry))
                    (lambda (_) (cdr entry))
                    result t t)))))

(defun org-lark--media-replacement (job err path st)
  "Compute the inline replacement string for a media JOB.
ERR is non-nil if the download failed; PATH is the local file path
on success.  ST is the export state, used for relative-path resolution."
  (let ((kind (plist-get job :kind))
        (token (plist-get job :token))
        (type (plist-get job :type))
        (attrs (plist-get job :attrs))
        (name (plist-get job :name)))
    (cond
     ((and (not err) path)
      (let ((rel (org-lark--relative-path path st))
            (attr-line (org-lark--attr-line attrs)))
        (pcase kind
          ('file (format "[[file:%s][%s]]\n" rel name))
          (_     (format "%s[[file:%s]]\n" attr-line rel)))))
     (t
      (pcase kind
        ('file (format "# Lark file token: %s name: %s\n"
                       (or token "") name))
        (_     (format "%s# Lark %s token: %s\n"
                       (org-lark--attr-line attrs)
                       (or type "image") (or token ""))))))))

(defun org-lark--substitute-media-results (text st results)
  "Replace media placeholders in TEXT using RESULTS for ST.
RESULTS is an alist of (KEY . (ERR . PATH))."
  (let ((result text))
    (dolist (entry (org-lark--state-media-jobs st) result)
      (let* ((key (car entry))
             (job (cdr entry))
             (download (cdr (assoc key results)))
             (err (car-safe download))
             (path (cdr-safe download))
             (replacement (org-lark--media-replacement job err path st)))
        (setq result (replace-regexp-in-string
                      (regexp-quote key)
                      (lambda (_) replacement)
                      result t t))))))

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
  "Show a status message built from FMT and ARGS in the minibuffer."
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

;;;; Async subprocess helpers ──────────────────────────────────────

(defun org-lark--run-async (program args callback)
  "Run PROGRAM with ARGS asynchronously.
Call CALLBACK with (ERR STDOUT) when the process exits.  ERR is
nil on success or a string describing the failure (timeout,
non-zero exit, etc.)."
  (let* ((bin (file-name-nondirectory program))
         (cmd (format "%s %s" bin
                      (mapconcat #'shell-quote-argument args " ")))
         (stdout-buf (generate-new-buffer " *org-lark*"))
         (stderr-buf (generate-new-buffer " *org-lark-err*"))
         (t0 (float-time))
         (timer nil)
         (proc nil))
    (org-lark--log "$ %s" cmd)
    (setq proc
          (make-process
           :name "org-lark-async"
           :buffer stdout-buf
           :stderr stderr-buf
           :command (cons program args)
           :connection-type 'pipe
           :noquery t
           :sentinel
           (lambda (p _event)
             (when (memq (process-status p) '(exit signal))
               (when timer (cancel-timer timer))
               (let* ((exit-code (process-exit-status p))
                      (out-len (with-current-buffer stdout-buf (buffer-size)))
                      (elapsed (- (float-time) t0)))
                 (org-lark--log "  → %.1fs, exit %d, %d bytes"
                                elapsed exit-code out-len)
                 (unwind-protect
                     (cond
                      ((zerop exit-code)
                       (let ((stdout (with-current-buffer stdout-buf
                                       (buffer-string))))
                         (funcall callback nil stdout)))
                      (t
                       (let ((err (with-current-buffer stderr-buf
                                    (string-trim (buffer-string)))))
                         (org-lark--log "  FAILED: %s" err)
                         (funcall callback
                                  (format "%s failed (exit %d): %s"
                                          bin exit-code err)
                                  nil))))
                   (ignore-errors (kill-buffer stdout-buf))
                   (ignore-errors (kill-buffer stderr-buf))))))))
    ;; Deadline: a non-blocking timer kills the process if it hangs.
    (setq timer
          (run-at-time
           org-lark-timeout nil
           (lambda ()
             (when (process-live-p proc)
               (org-lark--log "TIMEOUT %ds: %s" org-lark-timeout cmd)
               (delete-process proc)))))
    proc))

(defun org-lark--run-json-async (program args callback)
  "Run PROGRAM with ARGS, parse stdout as JSON, call CALLBACK with (ERR JSON)."
  (org-lark--run-async
   program args
   (lambda (err stdout)
     (cond
      (err (funcall callback err nil))
      (t (condition-case parse-err
             (funcall callback nil
                      (json-parse-string stdout
                                         :object-type 'alist
                                         :array-type 'list))
           (error
            (funcall callback
                     (format "JSON parse failed: %s"
                             (error-message-string parse-err))
                     nil))))))))

(defun org-lark--pandoc-async (markdown callback)
  "Async variant of `org-lark--pandoc'.
Call CALLBACK with (ERR ORG)."
  (org-lark--msg "running pandoc...")
  (let ((in-file (make-temp-file "org-lark-" nil ".md")))
    (with-temp-file in-file (insert markdown))
    (org-lark--run-async
     org-lark-pandoc-program
     (list "-f" "gfm" "-t" "org" "--wrap=none" in-file)
     (lambda (err out)
       (ignore-errors (delete-file in-file t))
       (funcall callback err out)))))

(defun org-lark-fetch-async (doc callback)
  "Async variant of `org-lark-fetch'.
Call CALLBACK with (ERR FETCHED-ALIST)."
  (org-lark--run-json-async
   org-lark-cli-program
   (list "docs" "+fetch" "--as" org-lark-identity
         "--doc" doc "--format" "json")
   (lambda (err json)
     (cond
      (err (funcall callback err nil))
      ((not (alist-get 'ok json))
       (funcall callback
                (format "lark-cli: %s"
                        (json-encode (or (alist-get 'error json) json)))
                nil))
      (t (let ((data (alist-get 'data json)))
           (funcall callback nil
                    `((markdown . ,(or (alist-get 'markdown data) ""))
                      (title    . ,(alist-get 'title data))
                      (doc_id   . ,(alist-get 'doc_id data))))))))))

(defun org-lark--download-media-async (token type st callback)
  "Async variant of `org-lark--download-media'.
Call CALLBACK with (ERR PATH).  PATH is nil on failure."
  (cond
   ((not (and org-lark-download-media token))
    (funcall callback nil nil))
   (t
    (make-directory (org-lark--state-asset-dir st) t)
    (let* ((output-dir (file-name-directory
                        (org-lark--state-output-file st)))
           (relative-output
            (file-name-as-directory
             (file-relative-name (org-lark--state-asset-dir st)
                                 output-dir)))
           (base-name (concat (org-lark--safe-filename token)
                              (when (string= type "whiteboard") "-wb")))
           (base (concat relative-output base-name))
           (args (append (list "docs" "+media-download"
                               "--as" org-lark-identity
                               "--token" token "--output" base)
                         (when type (list "--type" type))))
           (default-directory output-dir))
      (org-lark--log "media token=%s type=%s" token (or type "auto"))
      (org-lark--run-json-async
       org-lark-cli-program args
       (lambda (err json)
         (cond
          (err (org-lark--log "media FAILED: %s" err)
               (funcall callback err nil))
          ((not (alist-get 'ok json))
           (org-lark--log "media error token=%s" token)
           (funcall callback "media-download API error" nil))
          (t (let* ((data (alist-get 'data json))
                    (path (or (alist-get 'saved_path data)
                              (alist-get 'output data) base))
                    (path (if (file-name-absolute-p path)
                              path
                            (expand-file-name path output-dir))))
               (org-lark--log "  saved → %s" path)
               (funcall callback nil path))))))))))

(defun org-lark--download-all-media-async (st done-callback)
  "Run every queued media job in ST in parallel.
Call DONE-CALLBACK with the results alist when all jobs finish."
  (let* ((jobs (org-lark--state-media-jobs st))
         (n (length jobs))
         (remaining n)
         (results nil))
    (setf (org-lark--state-media-total st) n
          (org-lark--state-media-done st) 0)
    (cond
     ((zerop n) (funcall done-callback nil))
     (t
      (org-lark--msg "downloading %d media item(s)..." n)
      (dolist (entry jobs)
        (let ((key (car entry))
              (token (plist-get (cdr entry) :token))
              (type (plist-get (cdr entry) :type)))
          (org-lark--download-media-async
           token type st
           (lambda (err path)
             (push (cons key (cons err path)) results)
             (cl-incf (org-lark--state-media-done st))
             (cl-decf remaining)
             (let ((total (org-lark--state-media-total st))
                   (done (org-lark--state-media-done st)))
               (org-lark--msg "downloading media %d/%d..." done total))
             (when (zerop remaining)
               (funcall done-callback results))))))))))

;;;; Post-processing ───────────────────────────────────────────────

(defun org-lark--fix-deep-headings (text)
  "Convert deep Markdown headings in TEXT beyond level 6 to Org stars."
  (org-lark--re-replace
   text "^\\(?:\u200b\\)?\\(#\\{7,\\}\\)[ \t]+\\(.+\\)$"
   (lambda ()
     (concat (make-string (length (match-string 1)) ?*)
             " " (match-string 2)))))

(defun org-lark--normalize-attr-keyword-lines (text)
  "Ensure every `#+attr_org:' keyword in TEXT sits at column 0 on its own line.
Pandoc occasionally glues placeholder-restored `#+attr_org:' onto a
preceding list-bullet line or leaves it indented; either form breaks
the publish-side `^#+attr_org:' anchor and silently loses the
`lark-block' sentinel on a round-trip."
  ;; Inlined after non-newline text: "...bullet  #+attr_org: ..." \u2192 split.
  (setq text (replace-regexp-in-string
              "\\([^\n]\\)[ \t]+#\\+attr_org:" "\\1\n#+attr_org:" text))
  ;; Indented on its own line: "  #+attr_org: ..." \u2192 unindent.
  (setq text (replace-regexp-in-string
              "^[ \t]+#\\+attr_org:" "#+attr_org:" text))
  text)

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

(defun org-lark--with-sentinel (kind attrs)
  "Prepend a =lark-block=KIND= sentinel to the XML-ish ATTRS string.
Used so reverse-mapping can recognize a `quote'/`example' block as
the original Lark tag without relying on attribute heuristics."
  (concat (format "lark-block=\"%s\" " kind) (or attrs "")))

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

;;;; Publish (Org → Lark) ──────────────────────────────────────────
;;
;; Reverse pipeline: parse an Org file → strip metadata → reverse-map
;; #+begin_lark_* blocks and [[lark-*:..]] links back to Lark custom
;; tags → pandoc org→gfm → upload any new media → docs +create or
;; +update via lark-cli.  Mirrors the read path structure so the two
;; halves share helpers (placeholders, async runners, logging).

(cl-defstruct org-lark--pubstate
  "Mutable bag threaded through a single publish run."
  org-file body title doc-token parent
  (placeholders nil) (counter 0)
  ;; Media: each pending upload is (REL-PATH . PLIST), where PLIST has
  ;; :abs :token :attrs :name.  Resolved tokens accumulate in :tokens
  ;; as (REL-PATH . TOKEN) so the marker pass can substitute inline.
  (media-jobs nil) (media-tokens nil)
  ;; New tokens to persist back into the org file's #+attr_org lines.
  (new-attr-tokens nil))

(defvar org-lark--media-cache nil
  "Loaded media cache: alist of (KEY . TOKEN) where KEY is (ABS-PATH . SHA).")

(defvar org-lark--media-cache-loaded nil
  "Non-nil once `org-lark--media-cache' has been read from disk.")

(defun org-lark--media-cache-load ()
  "Lazily populate `org-lark--media-cache' from disk."
  (unless org-lark--media-cache-loaded
    (setq org-lark--media-cache-loaded t
          org-lark--media-cache
          (and (file-readable-p org-lark-media-cache-file)
               (with-temp-buffer
                 (insert-file-contents org-lark-media-cache-file)
                 (condition-case _ (read (current-buffer))
                   (error nil)))))))

(defun org-lark--media-cache-save ()
  "Persist `org-lark--media-cache' to disk."
  (condition-case err
      (let ((file org-lark-media-cache-file))
        (make-directory (file-name-directory file) t)
        (with-temp-file file
          (let ((print-length nil) (print-level nil))
            (prin1 org-lark--media-cache (current-buffer)))))
    (error (org-lark--log "media-cache save failed: %s"
                          (error-message-string err)))))

(defun org-lark--file-sha256 (path)
  "Return the SHA-256 digest of PATH's contents, or nil on error."
  (condition-case _
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally path)
        (secure-hash 'sha256 (current-buffer)))
    (error nil)))

(defun org-lark--media-cache-get (abs-path)
  "Return cached token for ABS-PATH, or nil."
  (org-lark--media-cache-load)
  (let ((sha (org-lark--file-sha256 abs-path)))
    (when sha
      (cdr (assoc (cons abs-path sha) org-lark--media-cache)))))

(defun org-lark--media-cache-put (abs-path token)
  "Store TOKEN for ABS-PATH in the persistent media cache."
  (when (and abs-path token)
    (let ((sha (org-lark--file-sha256 abs-path)))
      (when sha
        (org-lark--media-cache-load)
        (let ((key (cons abs-path sha)))
          (setq org-lark--media-cache
                (cons (cons key token)
                      (cl-remove-if (lambda (e) (equal (car e) key))
                                    org-lark--media-cache))))
        (org-lark--media-cache-save)))))

;;; Org header parsing

(defconst org-lark--affiliated-keywords
  '("caption" "data" "header" "headers" "label" "name" "plot"
    "results" "source" "srcname" "tblname")
  "Org affiliated keywords that bind to the following element.
These must not be consumed by `org-lark--parse-org-header', or the
attached block loses its metadata.  `attr_*' is handled separately
via prefix match since the suffix varies (`attr_org', `attr_html', …).")

(defun org-lark--header-keyword-p (kw)
  "Return non-nil if KW is a top-level keyword safe to absorb as a header.
Excludes affiliated keywords like =attr_org= / =caption= / =name=
that belong with the block immediately below them."
  (not (or (string-prefix-p "attr_" kw)
           (member kw org-lark--affiliated-keywords))))

(defun org-lark--parse-org-header (text)
  "Split TEXT into a (HEADER-ALIST . BODY) pair.
HEADER-ALIST contains values keyed by lowercased keyword strings
for leading =#+keyword: value= lines.  Affiliated keywords (see
`org-lark--affiliated-keywords' and the `attr_*' family) terminate
header parsing so they stay attached to their block in BODY."
  (let ((header nil) (lines (split-string text "\n")) (i 0))
    (cl-loop
     for line in lines
     while (or (string-match-p "^[ \t]*$" line)
               (and (string-match
                     "^#\\+\\([A-Za-z_][A-Za-z0-9_]*\\):[ \t]*\\(.*\\)$" line)
                    (org-lark--header-keyword-p
                     (downcase (match-string 1 line)))))
     do (progn
          (when (string-match
                 "^#\\+\\([A-Za-z_][A-Za-z0-9_]*\\):[ \t]*\\(.*\\)$" line)
            (push (cons (downcase (match-string 1 line))
                        (string-trim (match-string 2 line)))
                  header))
          (cl-incf i)))
    (cons (nreverse header)
          (string-trim (string-join (nthcdr i lines) "\n")))))

(defun org-lark--parse-parent (spec)
  "Parse SPEC like \"folder:TOKEN\" or \"wiki:SPACE/NODE\" into a plist."
  (cond
   ((or (null spec) (string-empty-p spec)) nil)
   ((string-match "^folder:\\(.+\\)$" spec)
    (list :folder-token (string-trim (match-string 1 spec))))
   ((string-match "^wiki:\\([^/]+\\)\\(?:/\\(.+\\)\\)?$" spec)
    (list :wiki-space (string-trim (match-string 1 spec))
          :wiki-node  (and (match-string 2 spec)
                           (string-trim (match-string 2 spec)))))
   (t (list :folder-token spec))))

;;; Reverse normalization (Org → Lark Markdown)

(defun org-lark--rev-ph (value st)
  "Push VALUE into ST and return a unique placeholder token.
Shares the format of read-path placeholders so we can reuse
`org-lark--restore-placeholders' verbatim."
  (let ((key (format "%s%dZ"
                     org-lark--ph-prefix
                     (cl-incf (org-lark--pubstate-counter st)))))
    (push (cons key value) (org-lark--pubstate-placeholders st))
    key))

(defun org-lark--rev-attr-pairs (attr-line)
  "Parse a leading =#+attr_org:= ATTR-LINE into an alist of (KEY . VALUE).
Splits on whitespace before `:KEY' tokens (Emacs has no lookahead)."
  (let (pairs)
    (when (and attr-line
               (string-match "^#\\+attr_org:[ \t]*\\(.*\\)$" attr-line))
      (let* ((rest (match-string 1 attr-line))
             ;; Insert a sentinel before every `:KEY' that follows
             ;; whitespace so we can split cleanly.
             (norm (replace-regexp-in-string
                    "[ \t]+:\\([A-Za-z][A-Za-z0-9_-]*\\)[ \t]+"
                    "\x01:\\1 " rest))
             (parts (split-string norm "\x01" t)))
        (dolist (part parts)
          (when (string-match "^:\\([A-Za-z][A-Za-z0-9_-]*\\)[ \t]+\\(.*\\)$"
                              part)
            (push (cons (match-string 1 part)
                        (string-trim (match-string 2 part)))
                  pairs)))))
    (nreverse pairs)))

(defun org-lark--rev-render-attrs (pairs &optional drop-keys)
  "Render PAIRS into an XML-ish attribute string.
DROP-KEYS is a list of attribute names to omit."
  (mapconcat
   (lambda (p)
     (format " %s=\"%s\"" (car p)
             (replace-regexp-in-string "\"" "&quot;" (cdr p))))
   (cl-remove-if (lambda (p) (member (car p) drop-keys)) pairs)
   ""))

(defun org-lark--rev-code-blocks (text st)
  "Replace =#+begin_src= blocks in TEXT with fenced placeholders.
ST collects placeholders so pandoc does not reflow code.
=#+begin_example= is intentionally left for
`org-lark--rev-blocks-with-attrs', which decides between callout
(when a =#+attr_org:= line carries Lark keys) and plain fenced
output."
  (org-lark--re-replace
   text
   "^#\\+begin_src\\(?:[ \t]+\\([^\n]*\\)\\)?\n\\(\\(?:.\\|\n\\)*?\\)\n#\\+end_src[ \t]*$"
   (lambda ()
     (let* ((info (or (match-string 1) ""))
            (body (match-string 2))
            (lang (when (string-match "^[[:alnum:]_+-]+" info)
                    (match-string 0 info))))
       (org-lark--rev-ph
        (format "\n```%s\n%s\n```\n" (or lang "") body)
        st)))))

(defun org-lark--rev-blocks-with-attrs (text st)
  "Replace =#+begin_…= blocks with reverse-mapped Lark tags in TEXT.
ST collects placeholders.  Handles =lark_grid=, =lark_column=,
=lark_chat_card=, =lark_addon=, =lark_source_synced=,
=lark_reference_synced=, =lark_view=, =example= (callout), =quote=."
  ;; Two anchored `^` clauses inside one regex (one in an optional
  ;; group, one not) silently fail to match in Emacs even when the
  ;; optional group is skipped, so the begin-line uses no anchor and
  ;; relies on the fact that code blocks were already protected.
  (let ((re (concat
             "\\(?:^#\\+attr_org:[ \t]*\\([^\n]*\\)\n\\)?"
             "#\\+begin_\\([A-Za-z_]+\\)[ \t]*\n"
             "\\(\\(?:.\\|\n\\)*?\\)\n"
             "#\\+end_\\2[ \t]*$")))
    (org-lark--re-replace
     text re
     (lambda ()
       (let* ((attr-line (and (match-string 1)
                              (concat "#+attr_org: " (match-string 1))))
              (name (match-string 2))
              (body (match-string 3))
              (pairs (org-lark--rev-attr-pairs attr-line)))
         (org-lark--rev-dispatch-block name pairs body st))))))

(defun org-lark--rev-dispatch-block (name pairs body st)
  "Return Lark-tag replacement for org block NAME with attribute PAIRS and BODY.
ST is the publish state."
  (pcase name
    ("lark_grid"
     (let ((inner (org-lark--rev-blocks-with-attrs body st)))
       (org-lark--rev-ph
        (format "<grid%s>\n%s\n</grid>"
                (org-lark--rev-render-attrs pairs)
                (string-trim inner))
        st)))
    ("lark_column"
     (org-lark--rev-ph
      (format "<column%s>\n%s\n</column>"
              (org-lark--rev-render-attrs pairs)
              (string-trim (org-lark--rev-blocks-with-attrs body st)))
      st))
    ("lark_chat_card"
     (org-lark--rev-ph
      (format "<chat-card%s/>" (org-lark--rev-render-attrs pairs)) st))
    ("lark_addon"
     (org-lark--rev-ph
      (format "<add-ons%s/>" (org-lark--rev-render-attrs pairs)) st))
    ("lark_source_synced"
     (org-lark--rev-ph
      (format "<source-synced%s>\n%s\n</source-synced>"
              (org-lark--rev-render-attrs pairs)
              (org-lark--rev-blocks-with-attrs body st))
      st))
    ("lark_reference_synced"
     (org-lark--rev-ph
      (format "<reference-synced%s>\n%s\n</reference-synced>"
              (org-lark--rev-render-attrs pairs)
              (org-lark--rev-blocks-with-attrs body st))
      st))
    ("lark_view"
     (org-lark--rev-ph
      (format "<view%s>\n%s\n</view>"
              (org-lark--rev-render-attrs pairs)
              (org-lark--rev-blocks-with-attrs body st))
      st))
    ("example"
     (let* ((sentinel (cdr (assoc "lark-block" pairs)))
            (others   (cl-remove-if (lambda (p) (string= (car p) "lark-block"))
                                    pairs)))
       (cond
        ;; New: explicit `lark-block="callout"' sentinel.
        ((equal sentinel "callout")
         (org-lark--rev-ph
          (format "<callout%s>\n%s\n</callout>"
                  (org-lark--rev-render-attrs others)
                  (string-trim (org-lark--rev-blocks-with-attrs body st)))
          st))
        ;; Legacy: pre-sentinel org files identified callouts heuristically
        ;; by the presence of any callout-only attribute.
        ((or (assoc "emoji" pairs) (assoc "background-color" pairs)
             (assoc "border-color" pairs))
         (org-lark--rev-ph
          (format "<callout%s>\n%s\n</callout>"
                  (org-lark--rev-render-attrs pairs)
                  (string-trim (org-lark--rev-blocks-with-attrs body st)))
          st))
        (t
         ;; Plain example → fall back to a fenced block.
         (org-lark--rev-ph
          (format "\n```\n%s\n```\n" body) st)))))
    ("quote"
     ;; Any `#+begin_quote' becomes a `<quote-container>'.  Strip the
     ;; sentinel (if present from a previous Lark export) so we don't
     ;; round-trip it back to Lark.
     (let ((others (cl-remove-if (lambda (p) (string= (car p) "lark-block"))
                                 pairs)))
       (org-lark--rev-ph
        (format "<quote-container%s>\n%s\n</quote-container>"
                (org-lark--rev-render-attrs others)
                (string-trim (org-lark--rev-blocks-with-attrs body st)))
        st)))
    (_
     ;; Unknown block: leave the original text untouched (re-emit).
     (let ((attr (if pairs
                     (format "#+attr_org:%s\n"
                             (org-lark--rev-render-attrs pairs))
                   "")))
       (format "%s#+begin_%s\n%s\n#+end_%s" attr name body name)))))

(defun org-lark--rev-images (text st)
  "Replace =[[file:..]]= references in TEXT with media markers.
Honours a preceding =#+attr_org: :token TOK= line: if present the
token is reused directly (emits <image token=…/>).  Otherwise the
asset is queued in ST for upload after the doc exists."
  (org-lark--re-replace
   text
   (concat
    "\\(?:#\\+attr_org:[ \t]*\\([^\n]*\\)\n\\)?"
    "\\[\\[file:\\([^]]+\\)\\]\\(?:\\[\\([^]]*\\)\\]\\)?\\]")
   (lambda ()
     (let* ((attr-line (and (match-string 1)
                            (concat "#+attr_org: " (match-string 1))))
            (path  (match-string 2))
            (label (match-string 3))
            (pairs (org-lark--rev-attr-pairs attr-line))
            (token (cdr (assoc "token" pairs)))
            (file-p (and label (not (string-empty-p label))
                         (not (member (file-name-extension path)
                                      '("png" "jpg" "jpeg" "gif" "webp"
                                        "svg" "bmp")))))
            (rel path)
            (abs (expand-file-name
                  path (file-name-directory
                        (org-lark--pubstate-org-file st)))))
       (cond
        ;; Already has a Lark token from a previous fetch: reuse.
        (token
         (org-lark--rev-ph
          (if file-p
              (format "<file token=\"%s\" name=\"%s\"/>"
                      token (or label ""))
            (format "<image token=\"%s\"%s/>"
                    token
                    (org-lark--rev-render-attrs pairs '("token"))))
          st))
        ;; Local asset: queue for upload, emit a stable marker token
        ;; that we substitute inline once a token is known.  The
        ;; marker is wrapped in a placeholder so pandoc does not
        ;; reflow or split it; placeholder restoration brings it
        ;; back into the MD before media substitution runs.
        ((file-readable-p abs)
         (let* ((marker (format "ORGLARKMEDIA__%d__"
                                (1+ (org-lark--pubstate-counter st))))
                (ph (org-lark--rev-ph marker st)))
           (push (cons rel
                       (list :abs abs :attrs pairs :name (or label "")
                             :kind (if file-p 'file 'image)
                             :marker marker))
                 (org-lark--pubstate-media-jobs st))
           ph))
        ;; Unreadable local path: keep the raw text so the user
        ;; notices.  Render as a Markdown link.
        (t
         (format "[%s](%s)" (or label rel) rel)))))))

(defun org-lark--rev-lark-links (text st)
  "Reverse-map Org Lark-* link syntax in TEXT to inline tags via ST."
  ;; mention-user
  (setq text
        (org-lark--re-replace
         text "\\[\\[lark-user:\\([^]]*\\)\\]\\[@?\\([^]]*\\)\\]\\]"
         (lambda ()
           (org-lark--rev-ph
            (format "<mention-user id=\"%s\"/>" (match-string 1))
            st))))
  ;; mention-doc
  (setq text
        (org-lark--re-replace
         text "\\[\\[lark-doc:\\([^]]+\\)\\]\\[\\([^]]*\\)\\]\\][ \t]*\\(?:# type: \\([^\n]*\\)\\)?"
         (lambda ()
           (let ((tok (match-string 1))
                 (label (match-string 2))
                 (type (or (match-string 3) "")))
             (org-lark--rev-ph
              (format "<mention-doc token=\"%s\" type=\"%s\">%s</mention-doc>"
                      tok (string-trim type) label)
              st)))))
  ;; lark-task
  (setq text
        (org-lark--re-replace
         text "\\[\\[lark-task:\\([^]]*\\)\\]\\[[^]]*\\]\\]"
         (lambda ()
           (org-lark--rev-ph
            (format "<task task-id=\"%s\"/>" (match-string 1)) st))))
  ;; lark-sheet
  (setq text
        (org-lark--re-replace
         text "\\[\\[lark-sheet:\\([^]]*\\)\\]\\[[^]]*\\]\\]"
         (lambda ()
           (org-lark--rev-ph
            (format "<sheet token=\"%s\"/>" (match-string 1)) st))))
  text)

(defun org-lark--rev-normalize (text st)
  "Run all reverse-mapping passes over TEXT, accumulating in ST."
  (let ((t0 (float-time)))
    (org-lark--log "publish: %d chars input" (length text))
    ;; Fix up `#+attr_org:` that pandoc may have glued onto a list line
    ;; or indented, so the block-recognizer sees it.
    (setq text (org-lark--normalize-attr-keyword-lines text))
    (setq text (org-lark--rev-code-blocks text st))
    (org-lark--log "  code blocks → %d placeholders"
                   (org-lark--pubstate-counter st))
    (setq text (org-lark--rev-blocks-with-attrs text st))
    (setq text (org-lark--rev-images text st))
    (setq text (org-lark--rev-lark-links text st))
    (org-lark--log "  reverse-normalize → %d placeholders, %d media jobs, %.2fs"
                   (org-lark--pubstate-counter st)
                   (length (org-lark--pubstate-media-jobs st))
                   (- (float-time) t0))
    text))

;;; Pandoc org→gfm

(defun org-lark--pandoc-org-to-md-async (org callback)
  "Async: convert ORG to GFM Markdown via pandoc, call CALLBACK (ERR MD)."
  (org-lark--msg "running pandoc...")
  (let ((in-file (make-temp-file "org-lark-pub-" nil ".org")))
    (with-temp-file in-file (insert org))
    (org-lark--log "pandoc org→gfm input (%d bytes, placeholders intact):\n---8<---\n%s\n--->8---"
                   (length org) org)
    (org-lark--run-async
     org-lark-pandoc-program
     (list "-f" "org" "-t" "gfm" "--wrap=none" in-file)
     (lambda (err out)
       (ignore-errors (delete-file in-file t))
       (unless err
         (org-lark--log "pandoc org→gfm output (%d bytes, before placeholder restore):\n---8<---\n%s\n--->8---"
                        (length out) out))
       (funcall callback err out)))))

;;; Media upload

(defun org-lark--upload-media-async (job doc-token callback)
  "Upload a single media JOB to the doc identified by DOC-TOKEN.
Calls CALLBACK with (ERR TOKEN).  Uses the on-disk media cache to
short-circuit unchanged files."
  (let* ((abs (plist-get (cdr job) :abs))
         (cached (org-lark--media-cache-get abs)))
    (cond
     (cached
      (org-lark--log "media cache hit %s → %s" abs cached)
      (funcall callback nil cached))
     (t
      (let* ((kind (plist-get (cdr job) :kind))
             (parent-type (if (eq kind 'file) "docx_file" "docx_image"))
             (args (list "docs" "+media-upload"
                         "--as" org-lark-identity
                         "--doc-id" doc-token
                         "--parent-type" parent-type
                         "--file" abs)))
        (org-lark--log "media upload %s (%s)" abs parent-type)
        (org-lark--run-json-async
         org-lark-cli-program args
         (lambda (err json)
           (org-lark--upload-media-handle-result
            err json abs callback))))))))

(defun org-lark--upload-media-handle-result (err json abs callback)
  "Process the lark-cli media-upload response and call CALLBACK (ERR TOKEN).
ABS is the local file path; on success it gets cached against the
returned token."
  (cond
   (err (funcall callback err nil))
   ((not (alist-get 'ok json))
    (funcall callback
             (format "media-upload: %s"
                     (json-encode (or (alist-get 'error json) json)))
             nil))
   (t (let* ((data (alist-get 'data json))
             (token (or (alist-get 'file_token data)
                        (alist-get 'token data)
                        (alist-get 'media_token data))))
        (cond
         (token (org-lark--media-cache-put abs token)
                (funcall callback nil token))
         (t (funcall callback
                     "media-upload: no token in response"
                     nil)))))))

(defun org-lark--upload-all-media-async (st doc-token done-callback)
  "Upload every queued job in ST against DOC-TOKEN, then call DONE-CALLBACK.
DONE-CALLBACK receives an alist of (REL-PATH . (ERR . TOKEN))."
  (let* ((jobs (org-lark--pubstate-media-jobs st))
         (n (length jobs))
         (remaining n)
         (results nil))
    (cond
     ((zerop n) (funcall done-callback nil))
     (t
      (org-lark--msg "uploading %d media item(s)..." n)
      (dolist (job jobs)
        (let ((rel (car job)))
          (org-lark--upload-media-async
           job doc-token
           (lambda (err token)
             (push (cons rel (cons err token)) results)
             (cl-decf remaining)
             (org-lark--msg "uploading media %d/%d..." (- n remaining) n)
             (when (zerop remaining)
               (funcall done-callback results))))))))))

(defun org-lark--substitute-media-tokens (text st results)
  "Replace media marker placeholders in TEXT using RESULTS for ST."
  (let ((out text))
    (dolist (job (org-lark--pubstate-media-jobs st) out)
      (let* ((rel (car job))
             (plist (cdr job))
             (marker (plist-get plist :marker))
             (kind (plist-get plist :kind))
             (name (plist-get plist :name))
             (attrs (plist-get plist :attrs))
             (download (cdr (assoc rel results)))
             (err (car-safe download))
             (token (cdr-safe download))
             (replacement
              (cond
               ((and (not err) token)
                (push (cons rel token)
                      (org-lark--pubstate-new-attr-tokens st))
                (cond
                 ((eq kind 'file)
                  (format "<file token=\"%s\" name=\"%s\"/>" token name))
                 (t
                  (format "<image token=\"%s\"%s/>" token
                          (org-lark--rev-render-attrs attrs '("token"))))))
               (t
                (org-lark--log "media failed for %s: %s" rel err)
                (format "[%s](%s)" (or name rel) rel)))))
        (setq out (replace-regexp-in-string
                   (regexp-quote marker)
                   (lambda (_) replacement)
                   out t t))))))

;;; lark-cli docs +create / +update wrappers

(defun org-lark--docs-create-async (title markdown parent callback)
  "Async wrapper for =docs +create=.  Calls CALLBACK with (ERR DATA-ALIST).
PARENT is the parsed plist from `org-lark--parse-parent' or nil."
  ;; lark-cli requires =--markdown @file= to be a relative path inside
  ;; the subprocess's cwd, so bind `default-directory' to the temp dir
  ;; and pass only the basename.
  (let* ((md-file (make-temp-file "org-lark-pub-" nil ".md"))
         (default-directory (file-name-directory md-file))
         (md-rel (file-name-nondirectory md-file)))
    (with-temp-file md-file (insert markdown))
    (let ((args (append
                 (list "docs" "+create"
                       "--as" org-lark-identity
                       "--title" title
                       "--markdown" (concat "@" md-rel))
                 (when (plist-get parent :folder-token)
                   (list "--folder-token" (plist-get parent :folder-token)))
                 (when (plist-get parent :wiki-space)
                   (list "--wiki-space" (plist-get parent :wiki-space)))
                 (when (plist-get parent :wiki-node)
                   (list "--wiki-node" (plist-get parent :wiki-node))))))
      (org-lark--run-json-async
       org-lark-cli-program args
       (lambda (err json)
         (ignore-errors (delete-file md-file t))
         (cond
          (err (funcall callback err nil))
          ((not (alist-get 'ok json))
           (funcall callback
                    (format "docs +create: %s"
                            (json-encode
                             (or (alist-get 'error json) json)))
                    nil))
          (t (funcall callback nil (alist-get 'data json)))))))))

(defun org-lark--docs-update-async (doc-token title markdown callback)
  "Async wrapper for =docs +update= in overwrite mode.
TITLE is non-nil to also rename the doc.  Calls CALLBACK (ERR DATA)."
  ;; See `org-lark--docs-create-async' for why we rebind `default-directory'.
  (let* ((md-file (make-temp-file "org-lark-pub-" nil ".md"))
         (default-directory (file-name-directory md-file))
         (md-rel (file-name-nondirectory md-file)))
    (with-temp-file md-file (insert markdown))
    (let ((args (append
                 (list "docs" "+update"
                       "--as" org-lark-identity
                       "--doc" doc-token
                       "--mode" org-lark-publish-update-mode
                       "--markdown" (concat "@" md-rel))
                 (when title (list "--new-title" title)))))
      (org-lark--run-json-async
       org-lark-cli-program args
       (lambda (err json)
         (ignore-errors (delete-file md-file t))
         (cond
          (err (funcall callback err nil))
          ((not (alist-get 'ok json))
           (funcall callback
                    (format "docs +update: %s"
                            (json-encode
                             (or (alist-get 'error json) json)))
                    nil))
          (t (funcall callback nil (alist-get 'data json)))))))))

;;; Header write-back (for newly-created docs)

(defun org-lark--write-header-back (org-file doc-id source-url)
  "Insert =#+lark_doc_id:= and =#+lark_source:= into ORG-FILE if missing."
  (when (and org-file (file-writable-p org-file))
    (with-temp-buffer
      (insert-file-contents org-file)
      (let ((dirty nil))
        (unless (save-excursion
                  (goto-char (point-min))
                  (re-search-forward "^#\\+lark_doc_id:" nil t))
          (goto-char (point-min))
          (if (re-search-forward "^#\\+title:.*\n" nil t)
              (progn (forward-line 0) (forward-line 1))
            (goto-char (point-min)))
          (insert (format "#+lark_doc_id: %s\n" doc-id))
          (setq dirty t))
        (unless (save-excursion
                  (goto-char (point-min))
                  (re-search-forward "^#\\+lark_source:" nil t))
          (goto-char (point-min))
          (if (re-search-forward "^#\\+lark_doc_id:.*\n" nil t)
              nil
            (goto-char (point-min)))
          (insert (format "#+lark_source: %s\n" source-url))
          (setq dirty t))
        (when dirty
          (write-region (point-min) (point-max) org-file nil 'quiet))))))

;;; Main publish pipeline

(defun org-lark--publish-pipeline-async (st callback)
  "Drive the full publish pipeline for ST, then call CALLBACK (ERR URL)."
  (let* ((body (org-lark--pubstate-body st))
         (md-with-markers (org-lark--rev-normalize body st)))
    (org-lark--pandoc-org-to-md-async
     md-with-markers
     (lambda (err md)
       (cond
        (err (funcall callback err nil))
        (t
         (let ((md (org-lark--restore-placeholders md st)))
           (org-lark--log "publish final markdown (%d bytes, after placeholder restore):\n---8<---\n%s\n--->8---"
                          (length md) md)
           (org-lark--publish-finish-async st md callback))))))))

(defun org-lark--publish-finish-async (st md callback)
  "Run create-or-update + media upload for ST and MD.  CALLBACK gets (ERR URL)."
  (let ((doc-token (org-lark--pubstate-doc-token st))
        (title (org-lark--pubstate-title st)))
    (cond
     ;; Update path: upload media first (we already have a doc token),
     ;; then push the body in one call.
     (doc-token
      (org-lark--upload-all-media-async
       st doc-token
       (lambda (results)
         (let ((md (org-lark--substitute-media-tokens md st results)))
           (org-lark--docs-update-async
            doc-token title md
            (lambda (err data)
              (cond
               (err (funcall callback err nil))
               (t (funcall callback nil
                           (or (alist-get 'url data)
                               (org-lark--pubstate-doc-token st)))))))))))
     ;; Create path: create with markers, then upload media against the
     ;; freshly minted doc token, then a second +update swaps the
     ;; markers for real <image token=…/> tags.
     (t
      (org-lark--docs-create-async
       title md (org-lark--pubstate-parent st)
       (lambda (err data)
         (cond
          (err (funcall callback err nil))
          (t
           (let* ((new-token (or (alist-get 'document_id data)
                                 (alist-get 'doc_id data)
                                 (alist-get 'token data)))
                  (url (alist-get 'url data)))
             (setf (org-lark--pubstate-doc-token st) new-token)
             (org-lark--write-header-back
              (org-lark--pubstate-org-file st) new-token (or url ""))
             (cond
              ;; No media to attach → first +create already wrote
              ;; everything (markers will remain visible in the doc;
              ;; user can rename / fix manually for v1).
              ((null (org-lark--pubstate-media-jobs st))
               (funcall callback nil url))
              (t
               (org-lark--upload-all-media-async
                st new-token
                (lambda (results)
                  (let ((md (org-lark--substitute-media-tokens
                             md st results)))
                    (org-lark--docs-update-async
                     new-token nil md
                     (lambda (err2 _data2)
                       (cond
                        (err2 (funcall callback err2 nil))
                        (t (funcall callback nil url)))))))))))))))))))

;;; Public commands

;;;###autoload
(defun org-lark-publish-async (org-file callback)
  "Publish ORG-FILE to Lark asynchronously.
CALLBACK is invoked with (ERR URL) on completion: ERR is nil on
success and URL is the doc URL (or token, if URL is unavailable);
on failure ERR is a descriptive string and URL is nil."
  (let* ((org-file (expand-file-name org-file))
         (text (with-temp-buffer
                 (insert-file-contents org-file)
                 (buffer-string)))
         (parsed (org-lark--parse-org-header text))
         (header (car parsed))
         (body   (cdr parsed))
         (title  (or (cdr (assoc "title" header))
                     (file-name-base org-file)))
         (doc-id (let ((v (cdr (assoc "lark_doc_id" header))))
                   (and v (not (string-empty-p v)) v)))
         (parent (org-lark--parse-parent
                  (or (cdr (assoc "lark_parent" header))
                      org-lark-publish-default-parent)))
         (st (make-org-lark--pubstate
              :org-file org-file :body body
              :title title :doc-token doc-id :parent parent)))
    (org-lark--log "publish %s → %s"
                   org-file (or doc-id "(create)"))
    (org-lark--msg "publishing %s..." (file-name-nondirectory org-file))
    (org-lark--publish-pipeline-async st callback)
    nil))

;;;###autoload
(defun org-lark-publish (&optional org-file)
  "Publish ORG-FILE (or the current buffer's file) to Lark.

When the file's =#+lark_doc_id:= header is set, the existing
remote document is updated with `org-lark-publish-update-mode'
(after a confirmation prompt if `org-lark-confirm-overwrite-remote'
is non-nil).  Otherwise a new document is created and the doc id
and URL are written back into the local file's header.

Runs asynchronously; the buffer stays responsive throughout."
  (interactive)
  (let* ((org-file (or org-file
                       (buffer-file-name)
                       (read-file-name "Org file to publish: ")))
         (text (with-temp-buffer
                 (insert-file-contents org-file)
                 (buffer-string)))
         (header (car (org-lark--parse-org-header text)))
         (doc-id (let ((v (cdr (assoc "lark_doc_id" header))))
                   (and v (not (string-empty-p v)) v))))
    (when (and doc-id
               org-lark-confirm-overwrite-remote
               (called-interactively-p 'interactive)
               (not (y-or-n-p
                     (format "Overwrite remote Lark doc %s? " doc-id))))
      (user-error "org-lark: publish cancelled"))
    (org-lark-publish-async
     org-file
     (lambda (err url)
       (cond
        (err (message "org-lark: publish failed: %s" err))
        (t (message "org-lark: published → %s" (or url "(no url)"))))))))

;;;###autoload
(defun org-lark-publish-buffer ()
  "Publish the current buffer (must be visiting a file) to Lark."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  (when (buffer-modified-p)
    (when (y-or-n-p "Buffer modified.  Save first? ")
      (save-buffer)))
  (org-lark-publish (buffer-file-name)))

(provide 'org-lark)

;;; org-lark.el ends here
