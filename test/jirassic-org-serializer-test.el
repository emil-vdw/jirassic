;;; jirassic-org-serializer-test.el --- Tests for jirassic-org-serializer -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'ert)

(require 'jirassic-org-serializer)

(ert-deftest jirassic-serializer-test-text-code ()
  (let ((link (make-jira-text
               :text "Order 66"
               :marks (list
                       (make-jira-mark
                        :type 'link
                        :attrs '((href . "https://acme.com/order-66/")))))))
    (should (string= (jirassic--serialize-to-org link)
                     "[[https://acme.com/order-66/][Order 66]]"))))

(ert-deftest jirassic-serializer-test-text-link ()
  (let ((link (make-jira-text
               :text "Order 66"
               :marks (list
                       (make-jira-mark
                        :type 'link
                        :attrs '((href . "https://acme.com/order-66/")))))))
    (should (string= (jirassic--serialize-to-org link 0)
                     "[[https://acme.com/order-66/][Order 66]]"))))

(ert-deftest jirassic-serializer-test-text-whitespace ()
  "Test that text formatted with emphasis markers handle whitespace correctly."
  ;; Leading whitespace
  (should (string=
           (jirassic--serialize-to-org
            (make-jira-text :text "  leading whitespace"
                            :marks (list (make-jira-mark :type 'strong))))
           "  *leading whitespace*"))

  ;; Trailing whitespace
  (should (string=
           (jirassic--serialize-to-org
            (make-jira-text :text "trailing whitespace   "
                            :marks (list (make-jira-mark :type 'em))))
           "/trailing whitespace/   "))

  ;; Leading and trailing whitespace
  (should (string=
           (jirassic--serialize-to-org
            (make-jira-text :text "  space "
                            :marks (list (make-jira-mark
                                          :type 'link
                                          :attrs '((href . "https://acme.com/"))))))
           "  [[https://acme.com/][space]] "))

  ;; Whitespace with no marker
  (should (string=
           (jirassic--serialize-to-org
            (make-jira-text :text "  space "))
           "  space ")))



(provide 'jirassic-org-serializer-test)
;;; jirassic-org-serializer-test.el ends here
