;;; jirassic-test.el --- Tests for jirassic

;;; Commentary:

;;; Code:
(require 'ert)
(require 'aio)
(require 'cl-lib)

(require 'jirassic-core)
(require 'jirassic-issue)
(require 'jirassic-parser)
(require 'jirassic-serializer)
(require 'jirassic-client)
(require 'jirassic-org)

;;; Test data
(require 'jirassic-conftest)
(require 'jirassic-complex-issue-data)

(defun jirassic-test--mock-get-issue (key)
  "Mock version of jirassic-get-issue."
  (let ((promise (aio-promise)))
    (cond
     ((string= key "KAN-1")
      (aio-resolve promise (lambda ()
                             (jirassic--parse-issue
                              jirassic-complex-issue-data))))
     ((string= key "NOTFOUND-1")
      (aio-resolve promise
                   (lambda ()
                     (signal 'jirassic-client-error
                             (make-jirassic-http-error
                              :message "Issue does not exist or you do not have permission to see it."
                              :code 404
                              :response nil)))))
     (t
      (aio-resolve promise
                   (lambda ()
                     (signal 'jirassic-client-error
                             (make-jirassic-http-error
                              :message "Generic test error"
                              :code 500
                              :response nil))))))
    promise))

(defmacro jirassic-test-capture (templates keys capture-file issue-data result-file)
  "Test that the capture matches the contents of RESULT-FILE."
  `(let ((expected-result (org-file-contents
                           ,result-file)))
     (unwind-protect
         (cl-letf (((symbol-function #'jirassic-get-issue)
                    (lambda (&rest args)
                      (let ((promise (aio-promise)))
                        (aio-resolve promise (lambda ()
                                               (jirassic--parse-issue
                                                ,issue-data)))
                        promise)))
                   (jirassic-org-capture-templates
                    ,templates)
                   (jirassic-org-add-attachments nil))

           (aio-wait-for (jirassic-org-capture "KAN-1" nil ,keys))

           (let ((captured-content (with-current-buffer (get-file-buffer ,capture-file)
                                     (buffer-string))))
             (should (string= captured-content expected-result)))))))


(ert-deftest jirassic-test-capture-complex-issue ()
  "Test successful capture of a complex Jira issue."
  (let ((temp-capture-file
         (make-temp-file "jirassic-test-capture-")))
    (jirassic-test-capture
     `(("t" "Todo" plain
        (file ,temp-capture-file)
        ,(concat
          "* %(issue-todo-state) %(issue-summary)\n"
          "%(issue-org-properties)\n%(issue-description)")))
     "t" temp-capture-file jirassic-complex-issue-data
     (f-join
      jirassic-fixture-location
      "complex-issue-result.org"))))

(ert-deftest jirassic-org-capture-select-key ()
  "Test selecting a specific template key."
  (let ((temp-capture-file (make-temp-file "jirassic-test-capture-key-")))
    (unwind-protect
        (cl-letf (((symbol-function #'jirassic-get-issue) #'jirassic-test--mock-get-issue)
                  (jirassic-org-capture-templates
                   `(("t" "Todo" plain (file ,temp-capture-file) "* TODO %(issue-summary) :T:")
                     ("n" "Note" plain (file ,temp-capture-file) "* NOTE %(issue-summary) :N:") ; Target this one
                     ))
                  (jirassic-org-add-attachments nil)
                  (jirassic-org-todo-state-alist nil)) ; Not needed for this template

          ;; Run the capture function, selecting the "n" key
          (aio-wait-for (jirassic-org-capture "KAN-1" nil "n")) ; Pass "n" as keys

          ;; Verify the content of the capture file
          (let ((captured-content (with-current-buffer (get-file-buffer temp-capture-file)
                                    (buffer-string))))
            ;; Check that the "Note" template was used
            (should (string-match-p "^\\* NOTE Test Issue for API Serialization :N:" captured-content))))
      ;; Cleanup
      (when (file-exists-p temp-capture-file)
        (delete-file temp-capture-file)))))

(ert-deftest jirassic-org-capture-issue-not-found ()
  "Test capture when the issue is not found (client error)."
  (cl-letf (((symbol-function #'jirassic-get-issue) #'jirassic-test--mock-get-issue))
    (should-error
     (aio-wait-for (jirassic-org-capture "NOTFOUND-1"))
     :type 'jirassic-client-error)
    ;; Additionally, check if the error message is propagated (optional)
    (condition-case err
        (aio-wait-for (jirassic-org-capture "NOTFOUND-1"))
      (jirassic-client-error
       (let ((http-error (cdr err)))
         ;; Ensure the error struct has the expected message from the mock
         (should (string= (jirassic-http-error-message http-error)
                          "Issue does not exist or you do not have permission to see it."))))
      (:errors (ert-fail "Signalled unexpected error type")))))

(ert-deftest jirassic-org-capture-generic-error ()
  "Test capture with a generic client error."
  (cl-letf (((symbol-function #'jirassic-get-issue) #'jirassic-test--mock-get-issue))
    (should-error (aio-wait-for (jirassic-org-capture "ANY-OTHER-KEY"))
                  :type 'jirassic-client-error)
    ;; Check the generic error message
    (condition-case err
        (aio-wait-for (jirassic-org-capture "ANY-OTHER-KEY"))
      (jirassic-client-error
       (should (string= (jirassic-http-error-message (cdr err))
                        "Generic test error")))
      (:errors (ert-fail "Signalled unexpected error type")))))

;;; jirassic-test.el ends here
