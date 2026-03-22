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

(provide 'jirassic-jira-parser-test)
;;; jirassic-jira-parser-test.el ends here
