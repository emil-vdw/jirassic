;;; jirassic-adf-parser-test.el --- Tests for jirassic-parser -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'ert)

(require 'jirassic-adf-parser)

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

(provide 'jirassic-adf-parser-test)
;;; jirassic-adf-parser-test.el ends here
