;;; org-lark-test.el --- Tests for org-lark -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org-lark)

(defmacro org-lark-test--with-pandoc-stub (&rest body)
  "Run BODY with a deterministic Pandoc stub."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'org-lark--run-pandoc)
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

(defun org-lark-test--ctx ()
  (make-org-lark--context
   :output-file (expand-file-name "out.org" temporary-file-directory)
   :asset-directory (expand-file-name "assets" temporary-file-directory)
   :placeholders nil
   :notices nil))

(ert-deftest org-lark-test-quote-and-callout ()
  (org-lark-test--with-pandoc-stub
    (let* ((ctx (org-lark-test--ctx))
           (org-lark-download-media nil)
           (out (org-lark-convert-markdown
                 "<quote-container>\n**hello**\n</quote-container>\n\n<callout emoji=\"dizzy\" background-color=\"light-orange\">\n# title\n</callout>"
                 ctx)))
      (should (string-match-p "#\\+begin_quote" out))
      (should (string-match-p "\\*hello\\*" out))
      (should (string-match-p "#\\+begin_lark_callout" out))
      (should (string-match-p ":emoji dizzy" out)))))

(ert-deftest org-lark-test-deep-heading-and-placeholder ()
  (org-lark-test--with-pandoc-stub
    (let* ((ctx (org-lark-test--ctx))
           (out (org-lark-convert-markdown
                 "####### heading 7\n\nUse <domain> and <missing_scope> literally."
                 ctx)))
      (should (string-match-p "^\\*\\*\\*\\*\\*\\*\\* heading 7" out))
      (should (string-match-p "<domain>" out))
      (should (string-match-p "<missing_scope>" out)))))

(ert-deftest org-lark-test-fenced-code-is-org-src ()
  (org-lark-test--with-pandoc-stub
    (let* ((ctx (org-lark-test--ctx))
           (out (org-lark-convert-markdown
                 "```python {wrap}\nprint('<domain>')\n```"
                 ctx)))
      (should (string-match-p "#\\+begin_src python" out))
      (should (string-match-p "print('<domain>')" out)))))

(ert-deftest org-lark-test-native-table ()
  (org-lark-test--with-pandoc-stub
    (let* ((ctx (org-lark-test--ctx))
           (out (org-lark-convert-markdown
                 "<lark-table rows=\"2\" cols=\"2\" header-row=\"true\">\n<lark-tr><lark-td>A</lark-td><lark-td>B</lark-td></lark-tr>\n<lark-tr><lark-td>x</lark-td><lark-td>y</lark-td></lark-tr>\n</lark-table>"
                 ctx)))
      (should (string-match-p "| A | B |" out))
      (should (string-match-p "|-" out))
      (should (string-match-p "| x | y |" out)))))

(ert-deftest org-lark-test-spanned-table-special-block ()
  (org-lark-test--with-pandoc-stub
    (let* ((ctx (org-lark-test--ctx))
           (org-lark-table-span-strategy 'special-block)
           (out (org-lark-convert-markdown
                 "<lark-table rows=\"1\" cols=\"2\">\n<lark-tr><lark-td colspan=\"2\">merged</lark-td></lark-tr>\n</lark-table>"
                 ctx)))
      (should (string-match-p "#\\+begin_lark_table" out))
      (should (string-match-p "colspan=\"2\"" out)))))

(ert-deftest org-lark-test-self-closing-links ()
  (org-lark-test--with-pandoc-stub
    (let* ((ctx (org-lark-test--ctx))
           (org-lark-download-media nil)
           (out (org-lark-convert-markdown
                 "<mention-user id=\"ou_abcdef123456\"/>\n<sheet token=\"sheet_1\"/>\n<task task-id=\"task_1\"/>\n<link-preview url=\"https%3A%2F%2Fexample.com\"/>"
                 ctx)))
      (should (string-match-p "\\[\\[lark-user:ou_abcdef123456\\]" out))
      (should (string-match-p "\\[\\[lark-sheet:sheet_1\\]" out))
      (should (string-match-p "\\[\\[lark-task:task_1\\]" out))
      (should (string-match-p "\\[\\[https://example.com\\]" out)))))

(ert-deftest org-lark-test-equation ()
  (org-lark-test--with-pandoc-stub
    (let* ((ctx (org-lark-test--ctx))
           (out (org-lark-convert-markdown
                 "<equation>f(n) = n + 1</equation>"
                 ctx)))
      (should (string-match-p "\\\\\\[" out))
      (should (string-match-p "f(n) = n \\+ 1" out))
      (should (string-match-p "\\\\\\]" out)))))

(ert-deftest org-lark-test-finalize-notice ()
  (let* ((ctx (make-org-lark--context
               :output-file "out.org"
               :asset-directory "assets"
               :placeholders nil
               :notices '(((_notice . nil)
                           (update . ((current . "1.0.9")
                                      (latest . "1.0.10")))))))
         (out (org-lark-finalize-org "* body" "Title" "doc" "docid" ctx)))
    (should (string-match-p "#\\+title: Title" out))
    (should (string-match-p "#\\+lark_doc_id: docid" out))
    (should (string-match-p "current 1.0.9 latest 1.0.10" out))))

(ert-deftest org-lark-test-fetch-pagination ()
  (let ((calls nil))
    (cl-letf (((symbol-function 'org-lark--run-json)
               (lambda (_program &rest args)
                 (push args calls)
                 (let ((offset (cadr (member "--offset" args))))
                   (if (string= offset "0")
                       '((ok . t)
                         (data . ((title . "Paged")
                                  (doc_id . "doc1")
                                  (markdown . "first")
                                  (has_more . t))))
                     '((ok . t)
                       (data . ((title . "Paged")
                                (doc_id . "doc1")
                                (markdown . "second")
                                (has_more . nil)))))))))
      (let ((fetched (org-lark-fetch "doc")))
        (should (equal (alist-get 'markdown fetched) "first\nsecond"))
        (should (equal (alist-get 'title fetched) "Paged"))
        (should (= (length calls) 2))))))

(ert-deftest org-lark-test-media-download-path ()
  (cl-letf (((symbol-function 'org-lark--run-json)
             (lambda (_program &rest _args)
               '((ok . t)
                 (data . ((saved_path . "/tmp/org-lark-asset.png")))))))
    (let ((ctx (org-lark-test--ctx))
          (org-lark-download-media t))
      (should (equal (org-lark--media-path "token" nil ctx)
                     "/tmp/org-lark-asset.png")))))

(provide 'org-lark-test)

;;; org-lark-test.el ends here
