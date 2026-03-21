;;; jirassic-adf-parser-test.el --- Tests for jirassic-parser -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'ert)

(require 'jirassic-adf-parser)

(defun jirassic-test--marks-equal (m1 m2)
  "Return t if `jira-mark' M1 is has identical values to M2."
  (and (cl-typep m1 'jira-mark) (cl-typep m2 'jira-mark)
       (eq (jira-mark-type m1) (jira-mark-type m2))
       (equal (jira-mark-attrs m1) (jira-mark-attrs m2))))

(ert-deftest jirassic-parser-test-parse-simple-text ()
  "Test that text without marks is parsed correctly."
  (let ((parsed-text
         (jirassic--parse-text
          '((type . "text")
            (text
             . "Monthly Wrap Email initiative doc")))))
    (should (string= (jira-text-text parsed-text)
                     "Monthly Wrap Email initiative doc"))
    (should (eq (jira-text-marks parsed-text) nil))))

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
    (should (string= (jira-text-text parsed-text)
                     "Monthly Wrap Email initiative doc"))
    (let ((marks (jira-text-marks parsed-text)))
      ;; backgroundColor
      (should (jirassic-test--marks-equal
               (nth 0 marks) (make-jira-mark :type 'backgroundColor :attrs nil)))
      ;; code
      (should (jirassic-test--marks-equal
               (nth 1 marks) (make-jira-mark :type 'code :attrs nil)))
      ;; em
      (should (jirassic-test--marks-equal
               (nth 2 marks) (make-jira-mark :type 'em :attrs nil)))
      ;; link
      (should (jirassic-test--marks-equal
               (nth 3 marks) (make-jira-mark  :type 'link
                                              :attrs '((href . "https://acme.com")))))
      ;; strike
      (should (jirassic-test--marks-equal
               (nth 4 marks) (make-jira-mark :type 'strike :attrs nil)))
      ;; strong
      (should (jirassic-test--marks-equal
               (nth 5 marks) (make-jira-mark :type 'strong :attrs nil)))
      ;; subsup
      (should (jirassic-test--marks-equal
               (nth 6 marks) (make-jira-mark :type 'subsup :attrs '((type . "sub")))))
      ;; textColor
      (should (jirassic-test--marks-equal
               (nth 7 marks) (make-jira-mark :type 'textColor :attrs nil)))
      ;; underline
      (should (jirassic-test--marks-equal
               (nth 8 marks) (make-jira-mark :type 'underline :attrs nil))))))

(provide 'jirassic-adf-parser-test)
;;; jirassic-adf-parser-test.el ends here
