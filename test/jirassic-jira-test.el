;;; jirassic-jira-test.el --- Tests for jirassic-jira -*- lexical-binding: t -*-

;;; Commentary:

;;; Test the Jira and ADF object utility functions.

;;; Code:
(require 'jirassic-jira)

(ert-deftest jirassic-jira--test-content-contains-node ()
  (should
   (jirassic--content-contains-node (list (make-adf-rule) (make-adf-table))
                                    '(adf-heading adf-table)))

  (should-not
   (jirassic--content-contains-node (list (make-adf-rule) (make-adf-table))
                                    nil))

  (should-not
   (jirassic--content-contains-node nil
                                    '(adf-heading adf-table)))

  (should-not
   (jirassic--content-contains-node nil nil)))

(provide 'jirassic-jira-test)
;;; jirassic-jira-test.el ends here
