;;; org-lark-test.el --- Tests for org-lark -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org-lark)

;;; Helpers ────────────────────────────────────────────────────────

(defmacro org-lark-test--with-pandoc-stub (&rest body)
  "Run BODY with a lightweight Pandoc stub (no subprocess)."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'org-lark--pandoc)
              (lambda (markdown)
                (let ((text markdown))
                  (setq text (replace-regexp-in-string
                              "^# \\(.+\\)$" "* \\1" text))
                  (setq text (replace-regexp-in-string
                              "^## \\(.+\\)$" "** \\1" text))
                  (setq text (replace-regexp-in-string
                              "\\*\\*\\([^*]+\\)\\*\\*" "*\\1*" text))
                  text))))
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
    (should (string-match-p "|-" out))
    (should (string-match-p "| x | y |" out))))

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
          (cl-letf (((symbol-function 'org-lark-fetch)
                     (lambda (_doc)
                       '((markdown . "# Hello")
                         (title . "My Doc")
                         (doc_id . "doc1"))))
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

(provide 'org-lark-test)

;;; org-lark-test.el ends here
