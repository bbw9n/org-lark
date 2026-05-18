;;; org-lark-test.el --- Tests for org-lark -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org-lark)

;;; Helpers ────────────────────────────────────────────────────────

(defmacro org-lark-test--with-pandoc-stub (&rest body)
  "Run BODY with a lightweight Pandoc stub (no subprocess).
Stubs both the sync `org-lark--pandoc' and the async
`org-lark--pandoc-async' so tests covering either flavour run
without spawning a real Pandoc process."
  (declare (indent 0))
  `(cl-letf* ((stub-fn (lambda (markdown)
                         (let ((text markdown))
                           (setq text (replace-regexp-in-string
                                       "^# \\(.+\\)$" "* \\1" text))
                           (setq text (replace-regexp-in-string
                                       "^## \\(.+\\)$" "** \\1" text))
                           (setq text (replace-regexp-in-string
                                       "\\*\\*\\([^*]+\\)\\*\\*" "*\\1*" text))
                           text)))
              ((symbol-function 'org-lark--pandoc) stub-fn)
              ((symbol-function 'org-lark--pandoc-async)
               (lambda (markdown cb)
                 (funcall cb nil (funcall stub-fn markdown)))))
     ,@body))

(defun org-lark-test--st ()
  "Create a throwaway state for tests."
  (make-org-lark--state
   :output-file (expand-file-name "out.org" temporary-file-directory)
   :asset-dir   (expand-file-name "assets" temporary-file-directory)))

(defun org-lark-test--convert (markdown)
  "Convenience: convert MARKDOWN with pandoc stub."
  (org-lark-test--with-pandoc-stub
    (let ((st (org-lark-test--st)))
      (org-lark--pipeline markdown
                          '((title . "Test") (doc_id . "d1"))
                          "test-url" st))))

(defun org-lark-test--arg-after (flag args)
  "Return the value after FLAG in ARGS."
  (cadr (member flag args)))

;;; Tests ──────────────────────────────────────────────────────────

(ert-deftest org-lark-test-quote-and-callout ()
  (let ((out (org-lark-test--convert
              "<quote-container>\n**hello**\n</quote-container>\n\n<callout emoji=\"dizzy\" background-color=\"light-orange\">\n# title\n</callout>")))
    (should (string-match-p "\\+begin_quote" out))
    (should (string-match-p "\\*hello\\*" out))
    (should (string-match-p "\\+begin_example" out))
    (should (string-match-p "\\+end_example" out))
    (should (string-match-p ":emoji dizzy" out))))

(ert-deftest org-lark-test-deep-heading-and-placeholder ()
  (let ((out (org-lark-test--convert
              "####### heading 7\n\nUse <domain> and <missing_scope> literally.")))
    (should (string-match-p "^\\*\\*\\*\\*\\*\\*\\* heading 7" out))
    (should (string-match-p "<domain>" out))
    (should (string-match-p "<missing_scope>" out))))

(ert-deftest org-lark-test-fenced-code-is-org-src ()
  (let ((out (org-lark-test--convert
              "```python {wrap}\nprint('<domain>')\n```")))
    (should (string-match-p "#\\+begin_src python" out))
    (should (string-match-p "print('<domain>')" out))))

(ert-deftest org-lark-test-native-table ()
  (let ((out (org-lark-test--convert
              "<lark-table rows=\"2\" cols=\"2\" header-row=\"true\">\n<lark-tr><lark-td>A</lark-td><lark-td>B</lark-td></lark-tr>\n<lark-tr><lark-td>x</lark-td><lark-td>y</lark-td></lark-tr>\n</lark-table>")))
    (should (string-match-p "| A | B |" out))
    (should (string-match-p "|---\\+---|" out))
    (should (string-match-p "| x | y |" out))))

(ert-deftest org-lark-test-table-column-alignment ()
  "Table columns should be padded to equal width."
  (let ((out (org-lark-test--convert
              "<lark-table header-row=\"true\">\n<lark-tr><lark-td>Name</lark-td><lark-td>X</lark-td></lark-tr>\n<lark-tr><lark-td>ab</lark-td><lark-td>cd</lark-td></lark-tr>\n</lark-table>")))
    ;; "ab" should be padded to match "Name" width (4)
    (should (string-match-p "| ab   | cd |" out))
    ;; Separator should span full column widths
    (should (string-match-p "|------\\+----" out))))

(ert-deftest org-lark-test-self-closing-links ()
  (org-lark-test--with-pandoc-stub
    (let* ((st (org-lark-test--st))
           (org-lark-download-media nil)
           (out (org-lark--pipeline
                 "<mention-user id=\"ou_abcdef123456\"/>\n<sheet token=\"sheet_1\"/>\n<task task-id=\"task_1\"/>\n<link-preview url=\"https%3A%2F%2Fexample.com\"/>"
                 '((title . "T") (doc_id . "d1")) "u" st)))
      (should (string-match-p "\\[\\[lark-user:ou_abcdef123456\\]" out))
      (should (string-match-p "\\[\\[lark-sheet:sheet_1\\]" out))
      (should (string-match-p "\\[\\[lark-task:task_1\\]" out))
      (should (string-match-p "\\[\\[https://example.com\\]" out)))))

(ert-deftest org-lark-test-equation ()
  (let ((out (org-lark-test--convert
              "<equation>f(n) = n + 1</equation>")))
    (should (string-match-p "\\\\\\[" out))
    (should (string-match-p "f(n) = n \\+ 1" out))
    (should (string-match-p "\\\\\\]" out))))

(ert-deftest org-lark-test-finalize-metadata ()
  (let ((out (org-lark-test--convert "hello")))
    (should (string-match-p "#\\+title: Test" out))
    (should (string-match-p "#\\+lark_doc_id: d1" out))
    (should (string-match-p "#\\+lark_source: test-url" out))))

(ert-deftest org-lark-test-fetch ()
  (cl-letf (((symbol-function 'org-lark--run-json)
             (lambda (_program &rest _args)
               '((ok . t)
                 (data . ((title . "My Doc") (doc_id . "doc1")
                          (markdown . "# Hello")))))))
    (let ((fetched (org-lark-fetch "doc")))
      (should (equal (alist-get 'markdown fetched) "# Hello"))
      (should (equal (alist-get 'title fetched) "My Doc"))
      (should (equal (alist-get 'doc_id fetched) "doc1")))))

(ert-deftest org-lark-test-noninteractive-export-does-not-open-file ()
  (let ((opened nil)
        (output (expand-file-name "org-lark-export-test.org"
                                  temporary-file-directory)))
    (unwind-protect
        (org-lark-test--with-pandoc-stub
          (cl-letf (((symbol-function 'org-lark-fetch-async)
                     (lambda (_doc cb)
                       (funcall cb nil
                                '((markdown . "# Hello")
                                  (title . "My Doc")
                                  (doc_id . "doc1")))))
                    ((symbol-function 'find-file)
                     (lambda (&rest _args)
                       (setq opened t))))
            (let ((org-lark-overwrite t)
                  (org-lark-open-after-export t)
                  (org-lark-download-media nil))
              (should (equal (org-lark-export "doc" output) output))
              (should-not opened)
              (should (file-exists-p output)))))
      (delete-file output t))))

(ert-deftest org-lark-test-media-download-path ()
  (let (seen-args)
    (cl-letf (((symbol-function 'org-lark--run-json)
             (lambda (_program &rest args)
               (setq seen-args args)
               '((ok . t)
                 (data . ((saved_path . "assets/tok.png")))))))
      (let ((st (org-lark-test--st))
            (org-lark-download-media t))
        (should (equal (org-lark--download-media "tok" nil st)
                       (expand-file-name "assets/tok.png" temporary-file-directory)))
        (should (equal (org-lark-test--arg-after "--output" seen-args)
                       "assets/tok"))
        (should-not (file-name-absolute-p
                     (org-lark-test--arg-after "--output" seen-args)))))))

(ert-deftest org-lark-test-image-token-downloads-to-embedded-file-link ()
  (let (seen-args)
    (org-lark-test--with-pandoc-stub
      (let* ((st (org-lark-test--st))
             (saved-path (expand-file-name "img-token.png"
                                           (org-lark--state-asset-dir st)))
             (org-lark-download-media t))
        (cl-letf (((symbol-function 'org-lark--run-json)
                   (lambda (_program &rest args)
                     (setq seen-args args)
                     `((ok . t)
                       (data . ((saved_path . ,saved-path)))))))
          (let* ((org-lark-download-media t)
                 (out (org-lark--pipeline
                       "<image token=\"img-token\" width=\"640\" align=\"center\"/>"
                       '((title . "T") (doc_id . "d1")) "u" st)))
            (should (member "+media-download" seen-args))
            (should (member "--token" seen-args))
            (should (member "img-token" seen-args))
            (should (equal (org-lark-test--arg-after "--output" seen-args)
                           "assets/img-token"))
            (should-not (file-name-absolute-p
                         (org-lark-test--arg-after "--output" seen-args)))
            (should-not (member "whiteboard" seen-args))
            (should (string-match-p "#\\+attr_org: .*:width 640" out))
            (should (string-match-p "\\[\\[file:assets/img-token.png\\]\\]" out))))))))

(ert-deftest org-lark-test-whiteboard-token-downloads-thumbnail-link ()
  (let (seen-args)
    (org-lark-test--with-pandoc-stub
      (let* ((st (org-lark-test--st))
             (saved-path (expand-file-name "wb-token.png"
                                           (org-lark--state-asset-dir st)))
             (org-lark-download-media t))
        (cl-letf (((symbol-function 'org-lark--run-json)
                   (lambda (_program &rest args)
                     (setq seen-args args)
                     `((ok . t)
                       (data . ((saved_path . ,saved-path)))))))
          (let* ((org-lark-download-media t)
                 (out (org-lark--pipeline
                       "<whiteboard token=\"wb-token\" align=\"left\"/>"
                       '((title . "T") (doc_id . "d1")) "u" st)))
            (should (member "+media-download" seen-args))
            (should (member "--token" seen-args))
            (should (member "wb-token" seen-args))
            (should (member "--type" seen-args))
            (should (member "whiteboard" seen-args))
            (should (equal (org-lark-test--arg-after "--output" seen-args)
                           "assets/wb-token-wb"))
            (should-not (file-name-absolute-p
                         (org-lark-test--arg-after "--output" seen-args)))
            (should (string-match-p "#\\+attr_org: .*:align left" out))
            (should (string-match-p "\\[\\[file:assets/wb-token.png\\]\\]" out))))))))

(ert-deftest org-lark-test-grid-and-columns ()
  (let ((out (org-lark-test--convert
              "<grid>\n<column width=\"50%\">\n**left**\n</column>\n<column width=\"50%\">\nright\n</column>\n</grid>")))
    (should (string-match-p "\\+begin_lark_grid" out))
    (should (string-match-p "\\+begin_lark_column" out))
    (should (string-match-p "\\*left\\*" out))
    (should (string-match-p "\\+end_lark_grid" out))))

(ert-deftest org-lark-test-quote-stripped-in-table ()
  "Quote/callout wrappers inside table cells are stripped."
  (let ((out (org-lark-test--convert
              "<lark-table header-row=\"true\">\n<lark-tr><lark-td>H</lark-td></lark-tr>\n<lark-tr><lark-td><quote-container>quoted text</quote-container></lark-td></lark-tr>\n</lark-table>")))
    (should (string-match-p "quoted text" out))
    (should-not (string-match-p "begin_quote" out))))

(ert-deftest org-lark-test-grid-in-table-to-list ()
  "A <lark-table> containing <grid> becomes a list, not an Org table."
  (let ((out (org-lark-test--convert
              (concat "<lark-table header-row=\"true\">"
                      "<lark-tr><lark-td>Name</lark-td><lark-td>Layout</lark-td></lark-tr>"
                      "<lark-tr><lark-td>Item</lark-td>"
                      "<lark-td><grid><column>left</column><column>right</column></grid></lark-td>"
                      "</lark-tr></lark-table>"))))
    (should (string-match-p "^- " out))
    (should-not (string-match-p "^| " out))
    (should (string-match-p "left" out))
    (should (string-match-p "right" out))))

(ert-deftest org-lark-test-table-in-grid-to-list ()
  "A <grid> containing <lark-table> becomes a list, not grid blocks."
  (let ((out (org-lark-test--convert
              (concat "<grid>"
                      "<column><lark-table><lark-tr>"
                      "<lark-td>A</lark-td><lark-td>B</lark-td>"
                      "</lark-tr></lark-table></column>"
                      "<column>text</column>"
                      "</grid>"))))
    (should (string-match-p "Column 1" out))
    (should (string-match-p "Column 2" out))
    (should-not (string-match-p "begin_lark_grid" out))
    (should (string-match-p "A" out))
    (should (string-match-p "B" out))))

(ert-deftest org-lark-test-async-defers-media-jobs ()
  "When `org-lark--defer-media' is bound, sc-media queues jobs
into ST.media-jobs instead of downloading."
  (let ((download-called 0))
    (cl-letf (((symbol-function 'org-lark--download-media)
               (lambda (&rest _) (cl-incf download-called) nil)))
      (let* ((st (org-lark-test--st))
             (org-lark-download-media t)
             (org-lark--defer-media t))
        (org-lark--normalize-tags
         "<image token=\"a\"/>\n<file token=\"b\" name=\"doc.pdf\"/>"
         st)
        (should (= download-called 0))
        (should (= (length (org-lark--state-media-jobs st)) 2))
        ;; Kinds recorded
        (let ((kinds (mapcar (lambda (e) (plist-get (cdr e) :kind))
                             (org-lark--state-media-jobs st))))
          (should (member 'image kinds))
          (should (member 'file kinds)))))))

(ert-deftest org-lark-test-async-pipeline-substitutes-media ()
  "Async pipeline emits placeholders during parse, downloads media
in parallel, and substitutes resolved paths into the final org."
  (let ((download-args nil)
        (output (expand-file-name "org-lark-async-test.org"
                                  temporary-file-directory)))
    (unwind-protect
        (org-lark-test--with-pandoc-stub
          (cl-letf (((symbol-function 'org-lark-fetch-async)
                     (lambda (_doc cb)
                       (funcall cb nil
                                '((markdown . "<image token=\"img1\" width=\"640\"/>")
                                  (title . "T") (doc_id . "d1")))))
                    ((symbol-function 'org-lark--download-media-async)
                     (lambda (token type st cb)
                       (push (list :token token :type type) download-args)
                       (funcall cb nil
                                (expand-file-name
                                 (concat token ".png")
                                 (org-lark--state-asset-dir st))))))
            (let ((org-lark-overwrite t)
                  (org-lark-download-media t)
                  (org-lark-open-after-export nil))
              (should (equal (org-lark-export "doc" output) output))
              ;; Async path was used: download-media-async invoked once.
              (should (= (length download-args) 1))
              (should (equal (plist-get (car download-args) :token) "img1"))
              ;; Final org contains the substituted file link.
              (let ((written (with-temp-buffer
                               (insert-file-contents output)
                               (buffer-string))))
                (should (string-match-p "\\[\\[file:assets/img1.png\\]\\]"
                                        written))
                (should (string-match-p "#\\+attr_org: .*:width 640"
                                        written))))))
      (ignore-errors (delete-file output t)))))

(ert-deftest org-lark-test-async-pipeline-fallback-on-download-error ()
  "When a media download fails, the placeholder substitutes the
fallback comment instead of a file link."
  (let ((output (expand-file-name "org-lark-async-fail-test.org"
                                  temporary-file-directory)))
    (unwind-protect
        (org-lark-test--with-pandoc-stub
          (cl-letf (((symbol-function 'org-lark-fetch-async)
                     (lambda (_doc cb)
                       (funcall cb nil
                                '((markdown . "<image token=\"x\"/>")
                                  (title . "T") (doc_id . "d1")))))
                    ((symbol-function 'org-lark--download-media-async)
                     (lambda (_token _type _st cb)
                       (funcall cb "boom" nil))))
            (let ((org-lark-overwrite t)
                  (org-lark-download-media t)
                  (org-lark-open-after-export nil))
              (org-lark-export "doc" output)
              (let ((written (with-temp-buffer
                               (insert-file-contents output)
                               (buffer-string))))
                (should (string-match-p "Lark image token: x" written))
                (should-not (string-match-p "\\[\\[file:" written))))))
      (ignore-errors (delete-file output t)))))

(ert-deftest org-lark-test-async-callback-receives-output-path ()
  "`org-lark-export-async' invokes its callback with (nil PATH) on success."
  (let ((output (expand-file-name "org-lark-cb-test.org"
                                  temporary-file-directory))
        seen-err seen-path)
    (unwind-protect
        (org-lark-test--with-pandoc-stub
          (cl-letf (((symbol-function 'org-lark-fetch-async)
                     (lambda (_doc cb)
                       (funcall cb nil
                                '((markdown . "# Hi")
                                  (title . "T") (doc_id . "d"))))))
            (let ((org-lark-overwrite t)
                  (org-lark-download-media nil))
              (org-lark-export-async
               "doc" output
               (lambda (err path)
                 (setq seen-err err seen-path path)))
              ;; Drain the event loop until the callback ran.
              (let ((deadline (+ (float-time) 2)))
                (while (and (not seen-path) (< (float-time) deadline))
                  (accept-process-output nil 0.05)))
              (should-not seen-err)
              (should (equal seen-path output))
              (should (file-exists-p output)))))
      (ignore-errors (delete-file output t)))))

(ert-deftest org-lark-test-single-pandoc-call ()
  "Ensure Pandoc is invoked exactly once, not once per block."
  (let ((pandoc-count 0))
    (cl-letf (((symbol-function 'org-lark--pandoc)
               (lambda (markdown)
                 (cl-incf pandoc-count)
                 markdown)))
      (let* ((st (org-lark-test--st))
             (md (mapconcat
                  (lambda (i)
                    (format "<quote-container>\npara %d\n</quote-container>\n" i))
                  (number-sequence 1 20) "\n")))
        (org-lark--pipeline md '((title . "T")) "u" st)
        (should (= pandoc-count 1))))))

;;; Publish path ───────────────────────────────────────────────────

(defun org-lark-test--pubst (&optional org-file)
  "Throwaway publish state pointing at ORG-FILE (or a tmp path)."
  (make-org-lark--pubstate
   :org-file (or org-file
                 (expand-file-name "in.org" temporary-file-directory))
   :body ""))

(defun org-lark-test--reverse (text &optional org-file)
  "Reverse-normalize TEXT and restore placeholders; helper for tests."
  (let* ((st (org-lark-test--pubst org-file))
         (out (org-lark--rev-normalize text st)))
    (org-lark--restore-placeholders out st)))

(ert-deftest org-lark-test-parse-org-header ()
  "Leading #+keywords parse into the header alist and strip from body."
  (let* ((text (concat "#+title: My doc\n"
                       "#+lark_doc_id: AB123\n"
                       "#+lark_source: https://example/doc\n"
                       "#+created_by: org-lark\n\n"
                       "Body line one.\n"))
         (parsed (org-lark--parse-org-header text)))
    (should (equal "My doc" (cdr (assoc "title" (car parsed)))))
    (should (equal "AB123" (cdr (assoc "lark_doc_id" (car parsed)))))
    (should (string-prefix-p "Body line one" (cdr parsed)))
    (should-not (string-match-p "#\\+title" (cdr parsed)))))

(ert-deftest org-lark-test-parse-parent ()
  "Folder / wiki parent specs round-trip through `org-lark--parse-parent'."
  (let ((folder (org-lark--parse-parent "folder:TOK1"))
        (wiki   (org-lark--parse-parent "wiki:SPACE_X/NODE_Y")))
    (should (equal "TOK1" (plist-get folder :folder-token)))
    (should-not (plist-get folder :wiki-space))
    (should (equal "SPACE_X" (plist-get wiki :wiki-space)))
    (should (equal "NODE_Y" (plist-get wiki :wiki-node)))))

(ert-deftest org-lark-test-reverse-code-block ()
  "Org src/example blocks reverse-map to fenced markdown."
  (let* ((org "#+begin_src python\nprint(1)\n#+end_src")
         (out (org-lark-test--reverse org)))
    (should (string-match-p "```python\nprint(1)\n```" out))))

(ert-deftest org-lark-test-reverse-grid ()
  "lark_grid + lark_column reverse to <grid>/<column>."
  (let* ((org (concat
               "#+begin_lark_grid\n"
               "#+begin_lark_column\n"
               "A\n"
               "#+end_lark_column\n"
               "#+begin_lark_column\n"
               "B\n"
               "#+end_lark_column\n"
               "#+end_lark_grid"))
         (out (org-lark-test--reverse org)))
    (should (string-match-p "<grid>" out))
    (should (string-match-p "<column>" out))
    (should (= 2 (cl-count-if
                  (lambda (l) (string-match-p "<column>" l))
                  (split-string out "\n"))))))

(ert-deftest org-lark-test-reverse-callout ()
  "example block with :emoji attr reverses to <callout …>."
  (let* ((org (concat
               "#+attr_org: :emoji 💡 :background-color light-yellow\n"
               "#+begin_example\n"
               "Tip body.\n"
               "#+end_example"))
         (out (org-lark-test--reverse org)))
    (should (string-match-p "<callout[^>]*emoji=\"💡\"" out))
    (should (string-match-p "background-color=\"light-yellow\"" out))
    (should (string-match-p "Tip body\\." out))))

(ert-deftest org-lark-test-reverse-mentions ()
  "lark-doc / lark-user / lark-task / lark-sheet links reverse to tags."
  (let ((out (org-lark-test--reverse
              (concat "[[lark-doc:TOK1][Some Doc]] # type: doc\n"
                      "[[lark-user:U_ABC][@user]]\n"
                      "[[lark-task:TID][Lark task]]\n"
                      "[[lark-sheet:STK][Lark sheet]]"))))
    (should (string-match-p
             "<mention-doc token=\"TOK1\" type=\"doc\">Some Doc</mention-doc>"
             out))
    (should (string-match-p "<mention-user id=\"U_ABC\"/>" out))
    (should (string-match-p "<task task-id=\"TID\"/>" out))
    (should (string-match-p "<sheet token=\"STK\"/>" out))))

(ert-deftest org-lark-test-reverse-image-with-token ()
  "[[file:..]] preceded by #+attr_org: :token T reuses the token."
  (let* ((org (concat
               "#+attr_org: :token TKN :width 640\n"
               "[[file:assets/foo.png]]"))
         (out (org-lark-test--reverse org)))
    (should (string-match-p "<image token=\"TKN\"" out))
    (should (string-match-p "width=\"640\"" out))
    (should-not (string-match-p "ORGLARKMEDIA" out))))

(ert-deftest org-lark-test-reverse-image-without-token-queues-upload ()
  "[[file:..]] without :token queues a media job and emits a marker."
  (let ((tmp (make-temp-file "org-lark-pub-img-" nil ".png")))
    (unwind-protect
        (let* ((org-file (make-temp-file "org-lark-pub-" nil ".org"))
               (rel (file-relative-name
                     tmp (file-name-directory org-file)))
               (st (org-lark-test--pubst org-file))
               (out (org-lark--rev-normalize
                     (format "[[file:%s]]" rel) st))
               (restored (org-lark--restore-placeholders out st))
               (jobs (org-lark--pubstate-media-jobs st)))
          (should (= 1 (length jobs)))
          (should (equal rel (caar jobs)))
          (should (string-match-p "ORGLARKMEDIA" restored))
          (should (eq 'image (plist-get (cdar jobs) :kind)))
          ;; The marker stored in the job matches the post-restoration
          ;; text, so substitution can locate it later.
          (should (string-match-p
                   (regexp-quote (plist-get (cdar jobs) :marker))
                   restored)))
      (ignore-errors (delete-file tmp t)))))

(ert-deftest org-lark-test-publish-create-routes-to-create ()
  "No #+lark_doc_id → docs +create is invoked with title + parent."
  (let* ((org-file (make-temp-file "org-lark-pub-" nil ".org"))
         (saw-create nil) (saw-update nil) (got-err nil) (got-url nil))
    (unwind-protect
        (progn
          (with-temp-file org-file
            (insert "#+title: Hello World\n\nBody.\n"))
          (cl-letf
              (((symbol-function 'org-lark--pandoc-org-to-md-async)
                (lambda (org cb) (funcall cb nil org)))
               ((symbol-function 'org-lark--docs-create-async)
                (lambda (title md _parent cb)
                  (setq saw-create (list :title title :md md))
                  (funcall cb nil
                           '((document_id . "NEW_DOC")
                             (url . "https://lark/NEW_DOC")))))
               ((symbol-function 'org-lark--docs-update-async)
                (lambda (&rest _args)
                  (setq saw-update t))))
            (org-lark-publish-async
             org-file
             (lambda (err url) (setq got-err err got-url url)))
            (let ((deadline (+ (float-time) 2)))
              (while (and (not got-url) (not got-err)
                          (< (float-time) deadline))
                (accept-process-output nil 0.05))))
          (should-not got-err)
          (should saw-create)
          (should-not saw-update)
          (should (equal "Hello World" (plist-get saw-create :title)))
          (should (equal "https://lark/NEW_DOC" got-url))
          ;; Header was written back into the org file.
          (with-temp-buffer
            (insert-file-contents org-file)
            (should (string-match-p "^#\\+lark_doc_id: NEW_DOC$"
                                    (buffer-string)))))
      (ignore-errors (delete-file org-file t)))))

(ert-deftest org-lark-test-publish-update-routes-to-update ()
  "#+lark_doc_id header → docs +update with --new-title."
  (let* ((org-file (make-temp-file "org-lark-pub-" nil ".org"))
         (saw-create nil) (saw-update nil) (got-url nil))
    (unwind-protect
        (progn
          (with-temp-file org-file
            (insert "#+title: New Title\n#+lark_doc_id: EXISTING\n\nBody.\n"))
          (cl-letf
              (((symbol-function 'org-lark--pandoc-org-to-md-async)
                (lambda (org cb) (funcall cb nil org)))
               ((symbol-function 'org-lark--docs-create-async)
                (lambda (&rest _) (setq saw-create t)))
               ((symbol-function 'org-lark--docs-update-async)
                (lambda (doc title md cb)
                  (setq saw-update (list :doc doc :title title :md md))
                  (funcall cb nil '((url . "https://lark/EXISTING"))))))
            (org-lark-publish-async
             org-file
             (lambda (_err url) (setq got-url url)))
            (let ((deadline (+ (float-time) 2)))
              (while (and (not got-url) (< (float-time) deadline))
                (accept-process-output nil 0.05))))
          (should-not saw-create)
          (should saw-update)
          (should (equal "EXISTING" (plist-get saw-update :doc)))
          (should (equal "New Title" (plist-get saw-update :title)))
          (should (equal "https://lark/EXISTING" got-url)))
      (ignore-errors (delete-file org-file t)))))

(ert-deftest org-lark-test-publish-cache-skips-upload ()
  "Cached media tokens are reused without invoking lark-cli."
  (let* ((png (make-temp-file "org-lark-pub-img-" nil ".png"))
         (org-file (make-temp-file "org-lark-pub-" nil ".org"))
         (rel (file-relative-name png (file-name-directory org-file)))
         (orig-cache (copy-tree org-lark--media-cache))
         (orig-loaded org-lark--media-cache-loaded)
         (got-url nil) (upload-calls 0))
    (unwind-protect
        (progn
          (with-temp-file png (insert "PNGDATA"))
          (with-temp-file org-file
            (insert (format "#+title: T\n#+lark_doc_id: D1\n\n[[file:%s]]\n" rel)))
          ;; Pre-seed cache for the file so reuse short-circuits upload.
          (let ((org-lark--media-cache-loaded t)
                (org-lark--media-cache nil))
            (org-lark--media-cache-put png "CACHED_TOK")
            (cl-letf
                (((symbol-function 'org-lark--pandoc-org-to-md-async)
                  (lambda (org cb) (funcall cb nil org)))
                 ((symbol-function 'org-lark--media-cache-save) #'ignore)
                 ((symbol-function 'org-lark--run-json-async)
                  (lambda (_p _a _cb) (cl-incf upload-calls)))
                 ((symbol-function 'org-lark--docs-update-async)
                  (lambda (_d _t md cb)
                    (should (string-match-p
                             "<image token=\"CACHED_TOK\"" md))
                    (funcall cb nil '((url . "u"))))))
              (org-lark-publish-async
               org-file (lambda (_e u) (setq got-url u)))
              (let ((deadline (+ (float-time) 2)))
                (while (and (not got-url) (< (float-time) deadline))
                  (accept-process-output nil 0.05))))
            (should (equal "u" got-url))
            (should (zerop upload-calls))))
      (setq org-lark--media-cache orig-cache
            org-lark--media-cache-loaded orig-loaded)
      (ignore-errors (delete-file png t))
      (ignore-errors (delete-file org-file t)))))

(provide 'org-lark-test)

;;; org-lark-test.el ends here
