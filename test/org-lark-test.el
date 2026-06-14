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

(ert-deftest org-lark-test-fetch-document-schema ()
  "Current lark-cli wraps content in data.document with a <title> prefix."
  (cl-letf (((symbol-function 'org-lark--run-json)
             (lambda (_program &rest _args)
               '((ok . t)
                 (data . ((document . ((document_id . "doc1")
                                       (revision_id . 12)
                                       (content . "<title>My Doc</title>\n\n# Hello")))))))))
    (let ((fetched (org-lark-fetch "doc")))
      (should (equal (alist-get 'markdown fetched) "# Hello"))
      (should (equal (alist-get 'title fetched) "My Doc"))
      (should (equal (alist-get 'doc_id fetched) "doc1")))))

(ert-deftest org-lark-test-fetch-heading-title ()
  "Markdown fetch may render the doc title as a leading # heading."
  (cl-letf (((symbol-function 'org-lark--run-json)
             (lambda (_program &rest _args)
               '((ok . t)
                 (data . ((document . ((document_id . "doc1")
                                       (content . "# My Doc\n\nBody text.")))))))))
    (let ((fetched (org-lark-fetch "doc")))
      (should (equal (alist-get 'markdown fetched) "Body text."))
      (should (equal (alist-get 'title fetched) "My Doc")))))

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
    (should (string-match-p "<img src=\"TKN\"" out))
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

(ert-deftest org-lark-test-docs-create-v2-args ()
  "+create uses v2 flags and prepends the title as a # heading."
  (let (saw-args saw-content got-data)
    (cl-letf (((symbol-function 'org-lark--run-json-async)
               (lambda (_program args cb)
                 (setq saw-args args)
                 (let ((file (substring (cadr (member "--content" args)) 1)))
                   (setq saw-content
                         (with-temp-buffer
                           (insert-file-contents (expand-file-name file))
                           (buffer-string))))
                 (funcall cb nil
                          '((ok . t)
                            (data . ((document . ((document_id . "d1")
                                                  (url . "https://lark/d1"))))))))))
      (org-lark--docs-create-async
       "My Doc" "Body." '(:folder-token "FLD1")
       (lambda (_err data) (setq got-data data))))
    (should (member "--doc-format" saw-args))
    (should (member "markdown" saw-args))
    (should (equal "FLD1" (cadr (member "--parent-token" saw-args))))
    (should-not (member "--title" saw-args))
    (should (equal "<title>My Doc</title>\n\nBody." saw-content))
    (should (equal "d1" (alist-get 'document_id got-data)))
    (should (equal "https://lark/d1" (alist-get 'url got-data)))))

(ert-deftest org-lark-test-docs-create-wiki-node-parent ()
  "wiki:SPACE/NODE maps the node token to --parent-token; space-only errors."
  (let (saw-args got-err)
    (cl-letf (((symbol-function 'org-lark--run-json-async)
               (lambda (_program args cb)
                 (setq saw-args args)
                 (funcall cb nil '((ok . t) (data . ((document))))))))
      (org-lark--docs-create-async
       "T" "B" (org-lark--parse-parent "wiki:SPACE_X/NODE_Y") #'ignore)
      (should (equal "NODE_Y" (cadr (member "--parent-token" saw-args))))
      (org-lark--docs-create-async
       "T" "B" (org-lark--parse-parent "wiki:SPACE_X")
       (lambda (err _data) (setq got-err err)))
      (should (stringp got-err)))))

(ert-deftest org-lark-test-docs-update-v2-args ()
  "+update uses --command/--content and renames via the leading heading."
  (let (saw-args saw-content)
    (cl-letf (((symbol-function 'org-lark--run-json-async)
               (lambda (_program args cb)
                 (setq saw-args args)
                 (let ((file (substring (cadr (member "--content" args)) 1)))
                   (setq saw-content
                         (with-temp-buffer
                           (insert-file-contents (expand-file-name file))
                           (buffer-string))))
                 (funcall cb nil
                          '((ok . t)
                            (data . ((document . ((document_id . "d1"))))))))))
      (org-lark--docs-update-async "EXISTING" "New Title" "Body." #'ignore))
    (should (equal "EXISTING" (cadr (member "--doc" saw-args))))
    (should (equal "overwrite" (cadr (member "--command" saw-args))))
    (should (equal "markdown" (cadr (member "--doc-format" saw-args))))
    (should-not (member "--new-title" saw-args))
    (should-not (member "--mode" saw-args))
    (should (equal "<title>New Title</title>\n\nBody." saw-content))))

(ert-deftest org-lark-test-publish-opens-url ()
  "Successful publish opens the doc URL in the browser; bare tokens don't."
  (let* ((org-file (make-temp-file "org-lark-pub-" nil ".org"))
         (opened nil)
         (result-url "https://lark/D1"))
    (unwind-protect
        (cl-letf (((symbol-function 'org-lark-publish-async)
                   (lambda (_file cb) (funcall cb nil result-url)))
                  ((symbol-function 'browse-url)
                   (lambda (url &rest _) (setq opened url))))
          (with-temp-file org-file (insert "#+title: T\n\nBody.\n"))
          (let ((org-lark-publish-open-url t))
            (org-lark-publish org-file)
            (should (equal "https://lark/D1" opened))
            ;; Token fallback (no URL) must not open a browser.
            (setq opened nil result-url "D1TOKEN")
            (org-lark-publish org-file)
            (should-not opened))
          (let ((org-lark-publish-open-url nil))
            (setq opened nil result-url "https://lark/D1")
            (org-lark-publish org-file)
            (should-not opened)))
      (ignore-errors (delete-file org-file t)))))

(ert-deftest org-lark-test-publish-update-routes-to-update ()
  "#+lark_doc_id header → docs +update with the new title."
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
            (org-lark--media-cache-put "D1" png "CACHED_TOK")
            (cl-letf
                (((symbol-function 'org-lark--pandoc-org-to-md-async)
                  (lambda (org cb) (funcall cb nil org)))
                 ((symbol-function 'org-lark--media-cache-save) #'ignore)
                 ((symbol-function 'org-lark--run-json-async)
                  (lambda (_p _a _cb) (cl-incf upload-calls)))
                 ((symbol-function 'org-lark--docs-update-async)
                  (lambda (_d _t md cb)
                    (should (string-match-p
                             "<img src=\"CACHED_TOK\"" md))
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

;;; Inline emphasis & CJK adjacency ────────────────────────────────

(ert-deftest org-lark-test-pad-cjk-emphasis ()
  "`org-lark--pad-cjk-emphasis' inserts spaces around CJK-adjacent emphasis."
  ;; Closing delimiter abutting a CJK letter.
  (should (equal "*项目：* 文本内容 X"
                 (org-lark--pad-cjk-emphasis "*项目：*文本内容 X")))
  ;; Opening delimiter abutting a CJK letter.
  (should (equal "用户 *重要* 然后"
                 (org-lark--pad-cjk-emphasis "用户*重要*然后")))
  ;; All five emphasis flavours are padded.
  (should (equal "X /斜/ Y"
                 (org-lark--pad-cjk-emphasis "X/斜/Y")))
  (should (equal "X =代码= Y"
                 (org-lark--pad-cjk-emphasis "X=代码=Y")))
  (should (equal "X +删除+ Y"
                 (org-lark--pad-cjk-emphasis "X+删除+Y")))
  ;; Pure ASCII context is left untouched.
  (should (equal "ASCII *bold* text"
                 (org-lark--pad-cjk-emphasis "ASCII *bold* text")))
  ;; Already-padded input is idempotent.
  (should (equal "*项目：* 文本内容"
                 (org-lark--pad-cjk-emphasis "*项目：* 文本内容"))))

(ert-deftest org-lark-test-header-skips-affiliated-keywords ()
  "`#+attr_org:' is an affiliated keyword and must stay attached to its block.
Earlier the header parser greedily ate any `#+keyword:' line, which
silently stripped the `:lark-block' sentinel from callouts."
  (let* ((text (concat "#+title: T\n"
                       "#+lark_doc_id: ABC\n\n"
                       "#+attr_org: :lark-block callout :emoji x\n"
                       "#+begin_example\n"
                       "body\n"
                       "#+end_example\n"))
         (parsed (org-lark--parse-org-header text))
         (header (car parsed))
         (body (cdr parsed)))
    (should (equal "T" (cdr (assoc "title" header))))
    (should-not (assoc "attr_org" header))
    (should (string-prefix-p "#+attr_org:" body))
    (should (string-match-p "#\\+begin_example" body))))

(ert-deftest org-lark-test-normalize-attr-keyword-lines ()
  "Inline / indented `#+attr_org:' is moved to its own column-0 line."
  ;; Inlined onto a list bullet (pandoc's list reflow).
  (should (equal "2. 有序列表 2-3\n#+attr_org: :emoji x\n#+begin_example"
                 (org-lark--normalize-attr-keyword-lines
                  "2. 有序列表 2-3 #+attr_org: :emoji x\n#+begin_example")))
  ;; Indented on its own line.
  (should (equal "#+attr_org: :width 33"
                 (org-lark--normalize-attr-keyword-lines
                  "   #+attr_org: :width 33"))))

(ert-deftest org-lark-test-inline-emphasis-publish-round-trip ()
  "Org emphasis (bold/italic/code/strike) converts to GFM via real pandoc."
  (skip-unless (executable-find "pandoc"))
  (let* ((org "Plain *bold*, /italic/, =code=, +strike+.")
         (st (org-lark-test--pubst))
         (md-with-markers (org-lark--rev-normalize org st))
         (tmp (make-temp-file "ol-test" nil ".org")))
    (with-temp-file tmp (insert md-with-markers))
    (unwind-protect
        (let* ((md (with-output-to-string
                     (with-current-buffer standard-output
                       (call-process "pandoc" nil t nil "-f" "org" "-t" "gfm"
                                     "--wrap=none" tmp))))
               (restored (org-lark--restore-placeholders md st)))
          (should (string-match-p "\\*\\*bold\\*\\*" restored))
          (should (string-match-p "\\*italic\\*" restored))
          (should (string-match-p "`code`" restored))
          (should (string-match-p "~~strike~~" restored)))
      (ignore-errors (delete-file tmp)))))

(ert-deftest org-lark-test-cjk-emphasis-read ()
  "CJK-adjacent markdown bold inside <callout> becomes Org bold."
  (skip-unless (executable-find "pandoc"))
  (let* ((md (concat "<callout emoji=\"x\">\n"
                     "**项目：**文本内容。\n"
                     "</callout>"))
         (st (make-org-lark--state
              :output-file (expand-file-name "x.org" temporary-file-directory)
              :asset-dir (expand-file-name "assets" temporary-file-directory)))
         (out (org-lark--pipeline md '((title . "T") (doc_id . "d1"))
                                  "u" st)))
    (should (string-match-p ":lark-block callout" out))
    (should (string-match-p "\\*项目：\\*" out))
    (should-not (string-match-p "\\*\\*项目" out))))

(ert-deftest org-lark-test-cjk-emphasis-publish ()
  "CJK-adjacent Org emphasis converts cleanly to GFM bold/italic on publish."
  (skip-unless (executable-find "pandoc"))
  (let* ((org "*项目：*文本内容。\n\n用户 /斜体/ 字段。")
         (st (org-lark-test--pubst))
         (md-with-markers (org-lark--rev-normalize org st))
         (tmp (make-temp-file "ol-test" nil ".org")))
    (with-temp-file tmp (insert md-with-markers))
    (unwind-protect
        (let* ((md (with-output-to-string
                     (with-current-buffer standard-output
                       (call-process "pandoc" nil t nil "-f" "org" "-t" "gfm"
                                     "--wrap=none" tmp))))
               (restored (org-lark--restore-placeholders md st)))
          (should (string-match-p "\\*\\*项目：\\*\\*" restored))
          (should (string-match-p "\\*斜体\\*" restored))
          ;; No pandoc-escaped asterisks should leak through.
          (should-not (string-match-p "\\\\\\*" restored)))
      (ignore-errors (delete-file tmp)))))

(ert-deftest org-lark-test-callout-body-emphasis-round-trip ()
  "Emphasis inside a callout body converts via pandoc (not held verbatim).
A previous implementation wrapped the whole callout in one placeholder
so the body bypassed pandoc; org `*X*' then went to Lark as italic,
not bold.  This test pins the inline-body wrapping."
  (skip-unless (executable-find "pandoc"))
  (let* ((body (concat
                "#+attr_org: :lark-block callout :emoji x\n"
                "#+begin_example\n"
                "*项目：*文本内容。\n"
                "#+end_example\n"))
         (st (org-lark-test--pubst))
         (md-with-markers (org-lark--rev-normalize body st))
         (tmp (make-temp-file "ol-test" nil ".org")))
    (with-temp-file tmp (insert md-with-markers))
    (unwind-protect
        (let* ((md (with-output-to-string
                     (with-current-buffer standard-output
                       (call-process "pandoc" nil t nil "-f" "org" "-t" "gfm"
                                     "--wrap=none" tmp))))
               (restored (org-lark--restore-placeholders md st)))
          (should (string-match-p "<callout[^>]*emoji=\"x\"" restored))
          (should (string-match-p "\\*\\*项目：\\*\\*" restored))
          (should (string-match-p "</callout>" restored))
          ;; The body must NOT survive as a single italicized `*项目：*'.
          (should-not (string-match-p "<callout[^>]*>\\(.\\|\n\\)*?\\*项目：\\*[^*]"
                                      restored)))
      (ignore-errors (delete-file tmp)))))

(ert-deftest org-lark-test-publish-pandoc-placeholder-survives ()
  "Placeholder tokens (`ORGLARKPH<N>Z') survive real pandoc untouched.
A trailing `_' would be GFM-escaped to `\\_', breaking restoration."
  (skip-unless (executable-find "pandoc"))
  (let* ((org (concat
               "#+attr_org: :lark-block callout :emoji x\n"
               "#+begin_example\n"
               "hello callout body\n"
               "#+end_example\n"))
         (st (org-lark-test--pubst))
         (md-with-markers (org-lark--rev-normalize org st))
         (tmp (make-temp-file "ol-test" nil ".org")))
    (with-temp-file tmp (insert md-with-markers))
    (unwind-protect
        (let* ((md (with-output-to-string
                     (with-current-buffer standard-output
                       (call-process "pandoc" nil t nil "-f" "org" "-t" "gfm"
                                     "--wrap=none" tmp))))
               (restored (org-lark--restore-placeholders md st)))
          (should (string-match-p "<callout[^>]*emoji=\"x\"" restored))
          (should (string-match-p "hello callout body" restored)))
      (ignore-errors (delete-file tmp)))))

(ert-deftest org-lark-test-inline-md-conversion-bypass-sites ()
  "Inline markdown converts to Org at every site that bypasses pandoc.
These tags assemble content directly into placeholders, so pandoc
never sees it; `org-lark--md-to-org-inline' is applied in each."
  ;; <text underline> body.
  (let ((out (org-lark-test--convert
              "<text underline=\"true\">**bold** and `code`</text>")))
    (should (string-match-p "_\\*bold\\* and =code=_" out)))
  ;; <agenda-item-title> / <agenda-item-content>.
  (let ((out (org-lark-test--convert
              (concat "<agenda><agenda-item>"
                      "<agenda-item-title>**Header**</agenda-item-title>"
                      "<agenda-item-content>`feat/foo`</agenda-item-content>"
                      "</agenda-item></agenda>"))))
    ;; Markdown `**` collapses to Org `*` in the title.
    (should-not (string-match-p "\\*\\*Header\\*\\*" out))
    (should (string-match-p "\\*Header\\*" out))
    (should (string-match-p "=feat/foo=" out)))
  ;; <okr-objective> body.
  (let ((out (org-lark-test--convert
              (concat "<okr period-name-zh=\"Q1\">"
                      "<okr-objective>**Core** Goal</okr-objective>"
                      "</okr>"))))
    (should (string-match-p "\\*Core\\* Goal" out)))
  ;; <mention-doc> label.
  (let ((out (org-lark-test--convert
              "<mention-doc token=\"TKN\" type=\"docx\">**Important** Doc</mention-doc>")))
    (should (string-match-p "\\*Important\\* Doc" out))))

(provide 'org-lark-test)

;;; org-lark-test.el ends here
