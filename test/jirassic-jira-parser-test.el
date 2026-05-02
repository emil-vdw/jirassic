;;; jirassic-jira-parser-test.el --- Tests for jirassic-jira-parser -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'ert)

(require 'jirassic-jira-parser)

(defun jirassic-test--marks-equal (m1 m2)
  "Return t if `adf-mark' M1 is has identical values to M2."
  (and (cl-typep m1 'adf-mark) (cl-typep m2 'adf-mark)
       (eq (adf-mark-type m1) (adf-mark-type m2))
       (equal (adf-mark-attrs m1) (adf-mark-attrs m2))))

(ert-deftest jirassic-parser-test-parse-simple-text ()
  "Test that text without marks is parsed correctly."
  (let ((parsed-text
         (jirassic--parse-text
          '((type . "text")
            (text
             . "Monthly Wrap Email initiative doc")))))
    (should (string= (adf-text-text parsed-text)
                     "Monthly Wrap Email initiative doc"))
    (should (eq (adf-text-marks parsed-text) nil))))

(ert-deftest jirassic-parser-test-parse-with-marks ()
  "Test that text with marks is parsed correctly."
  (let ((parsed-text
         (jirassic--parse-text
          '((type . "text")
            (text
             . "Monthly Wrap Email initiative doc")
            (marks
             . [((type . "backgroundColor"))
                ((type . "code"))
                ((type . "em"))
                ((type . "link")
                 (attrs
                  (href . "https://acme.com")))
                ((type . "strike"))
                ((type . "strong"))
                ((type . "subsup") (attrs . ((type . "sub"))))
                ((type . "textColor"))
                ((type . "underline"))])))))
    (should (string= (adf-text-text parsed-text)
                     "Monthly Wrap Email initiative doc"))
    (let ((marks (adf-text-marks parsed-text)))
      ;; backgroundColor
      (should (jirassic-test--marks-equal
               (nth 0 marks) (make-adf-mark :type 'backgroundColor)))
      ;; code
      (should (jirassic-test--marks-equal
               (nth 1 marks) (make-adf-mark :type 'code)))
      ;; em
      (should (jirassic-test--marks-equal
               (nth 2 marks) (make-adf-mark :type 'em)))
      ;; link
      (should (jirassic-test--marks-equal
               (nth 3 marks) (make-adf-mark  :type 'link
                                             :attrs '((href . "https://acme.com")))))
      ;; strike
      (should (jirassic-test--marks-equal
               (nth 4 marks) (make-adf-mark :type 'strike)))
      ;; strong
      (should (jirassic-test--marks-equal
               (nth 5 marks) (make-adf-mark :type 'strong)))
      ;; subsup
      (should (jirassic-test--marks-equal
               (nth 6 marks) (make-adf-mark :type 'subsup :attrs '((type . "sub")))))
      ;; textColor
      (should (jirassic-test--marks-equal
               (nth 7 marks) (make-adf-mark :type 'textColor)))
      ;; underline
      (should (jirassic-test--marks-equal
               (nth 8 marks) (make-adf-mark :type 'underline))))))

(ert-deftest jirassic-parser-test-parse-emoji ()
  "Test that emojis parsed correctly."
  (let ((emoji (jirassic-parse-adf-node '((type . "emoji")
                                          (attrs (shortName . ":thinking:")
                                                 (id . "1f914") (text . "🤔"))))))
    (should (cl-typep emoji 'adf-emoji))
    (should (string= (adf-emoji-text emoji) "🤔"))))

(ert-deftest jirassic-parser-test-parse-paragraph ()
  (let ((paragraph (jirassic--parse-paragraph
                    '((type . "paragraph")
                      (content
                       . [((type . "text")
                           (text . "Hello world"))])))))
    (should (cl-typep paragraph 'adf-paragraph))
    (should (cl-typep (car (adf-paragraph-content paragraph)) 'adf-text))
    (should (string= (adf-text-text (car (adf-paragraph-content paragraph)))
                     "Hello world"))))

(ert-deftest jirassic-parser-test-parse-code-block ()
  ;; With language
  (let ((code-block (jirassic--parse-code-block
                     '((type . "codeBlock")
                       (attrs (language . "python"))
                       (content
                        . [((type . "text")
                            (text . "class Foo:\n    x: int = 5\n\nf = Foo()"))])))))
    (should (cl-typep code-block 'adf-code-block))
    (should (string= (adf-code-block-language code-block) "python"))
    (should (string= (adf-text-text (car (adf-code-block-content code-block)))
                     "class Foo:\n    x: int = 5\n\nf = Foo()")))

  ;; Without language
  (let ((code-block (jirassic--parse-code-block
                     '((type . "codeBlock")
                       (content
                        . [((type . "text")
                            (text . "hello"))])))))
    (should (eq (adf-code-block-language code-block) nil))))

(ert-deftest jirassic-parser-test-parse-blockquote ()
  (let* ((blockquote (jirassic--parse-blockquote
                      '((type . "blockquote")
                        (content
                         . [((type . "paragraph")
                             (content
                              . [((type . "text")
                                  (text . "This is a"))]))
                            ((type . "paragraph")
                             (content
                              . [((type . "text")
                                  (text . "multiline quote"))]))]))))
         (content (adf-blockquote-content blockquote)))
    (should (cl-typep blockquote 'adf-blockquote))
    (should (= (length content) 2))
    (should (cl-typep (nth 0 content) 'adf-paragraph))
    (should (cl-typep (nth 1 content) 'adf-paragraph))
    (should (string= (adf-text-text (car (adf-paragraph-content (nth 0 content))))
                     "This is a"))
    (should (string= (adf-text-text (car (adf-paragraph-content (nth 1 content))))
                     "multiline quote"))))

(ert-deftest jirassic-parser-test-parse-expand ()
  (let* ((expand (jirassic--parse-expand
                  '((type . "expand")
                    (attrs (title . "Hello world"))
                    (content
                     . [((type . "paragraph")
                         (content
                          . [((type . "text")
                              (text . "Hello world"))]))]))))
         (content (adf-expand-content expand)))
    (should (cl-typep expand 'adf-expand))
    (should (string= (adf-expand-title expand) "Hello world"))
    (should (= (length content) 1))
    (should (cl-typep (nth 0 content) 'adf-paragraph)))

  ;; Title is optional — absent attrs.title should produce a nil title.
  (let ((expand (jirassic--parse-expand
                 '((type . "expand")
                   (attrs)
                   (content
                    . [((type . "paragraph")
                        (content . [((type . "text") (text . "x"))]))])))))
    (should (eq (adf-expand-title expand) nil))))

(ert-deftest jirassic-parser-test-parse-panel ()
  (let* ((panel (jirassic--parse-panel
                 '((type . "panel")
                   (attrs (panelType . "warning")
                          (localId . "088d70b6fe14"))
                   (content
                    . [((type . "paragraph")
                        (content
                         . [((type . "text")
                             (text . "Heads up!"))]))]))))
         (content (adf-panel-content panel)))
    (should (cl-typep panel 'adf-panel))
    (should (string= (adf-panel-panel-type panel) "warning"))
    (should (= (length content) 1))
    (should (cl-typep (nth 0 content) 'adf-paragraph))
    (should (string= (adf-text-text (car (adf-paragraph-content (nth 0 content))))
                     "Heads up!"))))

(ert-deftest jirassic-parser-test-parse-doc ()
  (let* ((doc (jirassic--parse-doc
               '((type . "doc")
                 (content
                  . [((type . "paragraph")
                      (content
                       . [((type . "text")
                           (text . "Hello world"))]))]))))
         (content (adf-doc-content doc)))
    (should (cl-typep doc 'adf-doc))
    (should (= (length content) 1))
    (should (cl-typep (car content) 'adf-paragraph))))

(ert-deftest jirassic-parser-test-parse-heading ()
  (let* ((heading (jirassic--parse-heading
                   '((type . "heading")
                     (attrs (level . 2))
                     (content
                      . [((type . "text")
                          (text . "Areas to investigate"))]))))
         (content (adf-heading-content heading)))
    (should (cl-typep heading 'adf-heading))
    (should (= (adf-heading-level heading) 2))
    (should (= (length content) 1))
    (should (cl-typep (car content) 'adf-text))
    (should (string= (adf-text-text (car content)) "Areas to investigate"))))

(ert-deftest jirassic-parser-test-parse-rule ()
  (let ((rule (jirassic-parse-adf-node '((type . "rule")))))
    (should (cl-typep rule 'adf-rule))))

(ert-deftest jirassic-parser-test-parse-bullet-list ()
  (let* ((bullet-list
          (jirassic-parse-adf-node
           '((type . "bulletList")
             (content
              . [((type . "listItem")
                  (content
                   . [((type . "paragraph")
                       (content . [((type . "text") (text . "First item"))]))]))
                 ((type . "listItem")
                  (content
                   . [((type . "paragraph")
                       (content . [((type . "text") (text . "Second item"))]))]))
                 ]))))
         (content (adf-bullet-list-content bullet-list)))
    (should (cl-typep bullet-list 'adf-bullet-list))
    (should (= (length content) 2))
    (should (cl-typep (nth 0 content) 'adf-list-item))
    (should (cl-typep (nth 1 content) 'adf-list-item))))

(ert-deftest jirassic-parser-test-parse-ordered-list ()
  (let* ((ordered-list
          (jirassic-parse-adf-node
           '((type . "orderedList")
             (content
              . [((type . "listItem")
                  (content
                   . [((type . "paragraph")
                       (content . [((type . "text") (text . "First item"))]))]))
                 ((type . "listItem")
                  (content
                   . [((type . "paragraph")
                       (content . [((type . "text") (text . "Second item"))]))]))
                 ]))))
         (content (adf-ordered-list-content ordered-list)))
    (should (cl-typep ordered-list 'adf-ordered-list))
    (should (= (length content) 2))
    (should (cl-typep (nth 0 content) 'adf-list-item))
    (should (cl-typep (nth 1 content) 'adf-list-item))))

(ert-deftest jirassic-parser-test-parse-list-item ()
  (let* ((list-item
          (jirassic-parse-adf-node
           '((type . "listItem")
             (content
              . [((type . "paragraph")
                  (content . [((type . "text") (text . "List item text"))]))]))))
         (content (adf-list-item-content list-item)))
    (should (cl-typep list-item 'adf-list-item))
    (should (= (length content) 1))
    (should (cl-typep (car content) 'adf-paragraph))
    (should (string= (adf-text-text (car (adf-paragraph-content (car content))))
                     "List item text"))))

(ert-deftest jirassic-parser-test-parse-table ()
  (let* ((table
          (jirassic--parse-table
           '((type . "table")
             (attrs (isNumberColumnEnabled . t))
             (content
              . [((type . "tableRow")
                  (content
                   . [((type . "tableHeader")
                       (content . [((type . "paragraph")
                                    (content . [((type . "text") (text . "Name"))]))]))
                      ((type . "tableCell")
                       (content . [((type . "paragraph")
                                    (content . [((type . "text") (text . "foo"))]))]))]))]))))
         (rows (adf-table-content table)))
    (should (cl-typep table 'adf-table))
    (should (eq (adf-table-is-numbere-columns-enabled table) t))
    (should (= (length rows) 1))
    (let ((cells (adf-table-row-content (car rows))))
      (should (cl-typep (nth 0 cells) 'adf-table-header))
      (should (cl-typep (nth 1 cells) 'adf-table-cell))
      (should (string= (adf-text-text
                        (car (adf-paragraph-content
                              (car (adf-table-cell-content (nth 1 cells))))))
                       "foo")))))

(ert-deftest jirassic-parser-test-parse-table-row ()
  (let* ((row (jirassic-parse-adf-node
               '((type . "tableRow")
                 (content
                  . [((type . "tableCell")
                      (content . [((type . "paragraph")
                                   (content . [((type . "text") (text . "A"))]))]))
                     ((type . "tableCell")
                      (content . [((type . "paragraph")
                                   (content . [((type . "text") (text . "B"))]))]))]))))
         (cells (adf-table-row-content row)))
    (should (cl-typep row 'adf-table-row))
    (should (= (length cells) 2))
    (should (cl-typep (nth 0 cells) 'adf-table-cell))
    (should (cl-typep (nth 1 cells) 'adf-table-cell))))

(ert-deftest jirassic-parser-test-parse-table-header ()
  ;; adf-table-header has no content slot; verify it parses without error
  (let ((header (jirassic-parse-adf-node
                 '((type . "tableHeader")
                   (content . [((type . "paragraph")
                                (content . [((type . "text") (text . "Col"))]))])))))
    (should (cl-typep header 'adf-table-header))))

(ert-deftest jirassic-parser-test-parse-table-cell ()
  (let* ((cell (jirassic-parse-adf-node
                '((type . "tableCell")
                  (content . [((type . "paragraph")
                               (content . [((type . "text") (text . "hello"))]))]))))
         (content (adf-table-cell-content cell)))
    (should (cl-typep cell 'adf-table-cell))
    (should (= (length content) 1))
    (should (cl-typep (car content) 'adf-paragraph))
    (should (string= (adf-text-text (car (adf-paragraph-content (car content))))
                     "hello"))))

(ert-deftest jirassic-parser-test-parse-inline-card ()
  ;; URL variant
  (let ((card (jirassic-parse-adf-node '((type . "inlineCard")
                                         (attrs (url . "https://acme.com"))))))
    (should (cl-typep card 'adf-inline-card))
    (should (string= (adf-inline-card-url card) "https://acme.com"))
    (should (eq (adf-inline-card-data card) nil)))

  ;; Data variant
  (let* ((data '((@context . "https://schema.org")
                 (@type . "DigitalDocument")
                 (name . "My Confluence Page")))
         (card (jirassic-parse-adf-node `((type . "inlineCard")
                                          (attrs (data . ,data))))))
    (should (cl-typep card 'adf-inline-card))
    (should (eq (adf-inline-card-url card) nil))
    (should (equal (adf-inline-card-data card) data))))

(ert-deftest jirassic-parser-test-parse-issue ()
  (let ((issue (jirassic--parse-issue
                '((id . "10001")
                  (key . "PROJ-1")
                  (fields
                   (summary . "Fix the bug")
                   (status (name . "In Progress"))
                   (issuetype (name . "Bug"))
                   (creator (accountId . "abc123")
                             (displayName . "Jane Doe")
                             (emailAddress . "jane@example.com"))
                   (priority (name . "High"))
                   (project (id . "10000")
                             (key . "PROJ")
                             (name . "My Project"))
                   (description
                    (type . "doc")
                    (content
                     . [((type . "paragraph")
                         (content . [((type . "text")
                                      (text . "Bug description"))]))])))))))
    (should (cl-typep issue 'jira-issue))
    (should (string= (jira-issue-id issue) "10001"))
    (should (string= (jira-issue-key issue) "PROJ-1"))
    (should (string= (jira-issue-summary issue) "Fix the bug"))
    (should (string= (jira-issue-status issue) "In Progress"))
    (should (string= (jira-issue-type issue) "Bug"))
    (should (string= (jira-issue-priority issue) "High"))
    (should (cl-typep (jira-issue-description issue) 'adf-doc))
    (let ((creator (jira-issue-creator issue)))
      (should (cl-typep creator 'jira-user))
      (should (string= (jira-user-display-name creator) "Jane Doe"))
      (should (string= (jira-user-email creator) "jane@example.com")))
    (let ((project (jira-issue-project issue)))
      (should (cl-typep project 'jira-project))
      (should (string= (jira-project-id project) "10000"))
      (should (string= (jira-project-key project) "PROJ"))
      (should (string= (jira-project-name project) "My Project")))))

(provide 'jirassic-jira-parser-test)
;;; jirassic-jira-parser-test.el ends here
