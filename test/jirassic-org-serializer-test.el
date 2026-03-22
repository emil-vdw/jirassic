;;; jirassic-org-serializer-test.el --- Tests for jirassic-org-serializer -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'ert)

(require 'jirassic-org-serializer)

(ert-deftest jirassic-serializer-test-text-code ()
  (let ((link (make-adf-text
               :text "Order 66"
               :marks (list
                       (make-adf-mark
                        :type 'link
                        :attrs '((href . "https://acme.com/order-66/")))))))
    (should (string= (jirassic--serialize-to-org link)
                     "[[https://acme.com/order-66/][Order 66]]"))))

(ert-deftest jirassic-serializer-test-text-link ()
  (let ((link (make-adf-text
               :text "Order 66"
               :marks (list
                       (make-adf-mark
                        :type 'link
                        :attrs '((href . "https://acme.com/order-66/")))))))
    (should (string= (jirassic--serialize-to-org link 0)
                     "[[https://acme.com/order-66/][Order 66]]"))))

(ert-deftest jirassic-serializer-test-text-whitespace ()
  "Test that text formatted with emphasis markers handle whitespace correctly."
  ;; Leading whitespace
  (should (string=
           (jirassic--serialize-to-org
            (make-adf-text :text "  leading whitespace"
                            :marks (list (make-adf-mark :type 'strong))))
           "  *leading whitespace*"))

  ;; Trailing whitespace
  (should (string=
           (jirassic--serialize-to-org
            (make-adf-text :text "trailing whitespace   "
                            :marks (list (make-adf-mark :type 'em))))
           "/trailing whitespace/   "))

  ;; Leading and trailing whitespace
  (should (string=
           (jirassic--serialize-to-org
            (make-adf-text :text "  space "
                            :marks (list (make-adf-mark
                                          :type 'link
                                          :attrs '((href . "https://acme.com/"))))))
           "  [[https://acme.com/][space]] "))

  ;; Whitespace with no marker
  (should (string=
           (jirassic--serialize-to-org
            (make-adf-text :text "  space "))
           "  space ")))

(ert-deftest jirassic-serializer-test-bullet-list ()
  (let ((bullet-list (make-adf-bullet-list
                      :content (list (make-adf-list-item
                                      :content (make-adf-text :text "first bullet"))
                                     (make-adf-list-item
                                      :content (make-adf-text :text "second bullet"))
                                     (make-adf-list-item
                                      :content (make-adf-text :text "third bullet"))))))
    (should (string= (jirassic--serialize-to-org bullet-list)
                     "- first bullet\n- second bullet\n- third bullet"))))

(ert-deftest jirassic-serializer-test-date ()
  (should (string= (jirassic--serialize-to-org
                    (make-adf-date :timestamp "1582152559"))
                   "<2020-02-19 Wed>"))

  ;; Test the empty string case since the API doesn't gaurentee a non-empty string.
  (should (string= (jirassic--serialize-to-org
                    (make-adf-date :timestamp ""))
                   "<1970-01-01 Thu>")))

(ert-deftest jirassic-serializer-test-hard-break ()
  (should (string= (jirassic--serialize-to-org (make-adf-hard-break))
                   "\n")))

(ert-deftest jirassic-serializer-test-rule ()
  (should (string= (jirassic--serialize-to-org (make-adf-rule))
                   "-----")))


(provide 'jirassic-org-serializer-test)
;;; jirassic-org-serializer-test.el ends here
