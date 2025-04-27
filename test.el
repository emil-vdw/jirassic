;;; jirassic-org-capture-tests.el --- Tests for jirassic-org-capture -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'ert)
(require 'aio)
(require 'cl-lib)

;; Load the necessary Jirassic files
(require 'jirassic-core)
(require 'jirassic-issue)
(require 'jirassic-parser)
(require 'jirassic-serializer)
(require 'jirassic-client)
(require 'jirassic-org)

(defvar jirassic-test--mock-issue-data
  `((self . "http://jira.example.com/rest/api/3/issue/10001")
    (id . "10001")
    (key . "TEST-1")
    (fields . ((summary . "This is a test summary")
               (description . ((type . "doc")
                               (version . 1)
                               (content . [((type . "paragraph")
                                            (content . [((type . "text")
                                                         (text . "Test description content."))]))])))
               (status . ((name . "In Progress")))
               (creator . ((self . "http://jira.example.com/rest/api/3/user?accountId=...")
                           (accountId . "id-creator")
                           (emailAddress . "creator@example.com")
                           (displayName . "Test Creator")))
               (project . ((key . "TESTPROJ")))
               (issuetype . ((name . "Task")))
               (priority . ((name . "Medium")))
               (attachment . []))))
  "Mock raw data returned by Jira API for TEST-1.")

(defvar jirassic-test--mock-issue
  (jirassic--parse-issue jirassic-test--mock-issue-data)
  "Mock parsed jirassic-issue struct for TEST-1.")

(defun jirassic-test--mock-get-issue (key)
  "Mock version of jirassic-get-issue."
  (let ((promise (aio-promise)))
    (cond
     ((string= key "TEST-1")
      (aio-resolve promise (lambda ()
                             (jirassic--parse-issue
                              jirassic-test--mock-issue-data)))) ; Return raw data
     ((string= key "NOTFOUND-1")
      (aio-resolve promise
                   (lambda ()
                     (signal 'jirassic-client-error
                             (make-jirassic-http-error
                              :message "Issue does not exist or you do not have permission to see it."
                              :code 404
                              :response nil))))) ; Simulate 404
     (t
      (aio-resolve promise
                   (lambda ()
                     (signal 'jirassic-client-error
                             (make-jirassic-http-error
                              :message "Generic test error"
                              :code 500
                              :response nil))))) ; Simulate other error
     )
    promise))

;; --- Test Cases ---

(ert-deftest jirassic-org-capture-success ()
  "Test successful capture of a Jira issue."
  (let ((temp-capture-file (make-temp-file "jirassic-test-capture-")))
    (unwind-protect
        (cl-letf (((symbol-function #'jirassic-get-issue) #'jirassic-test--mock-get-issue)
                  (jirassic-org-capture-templates
                   `(("t" "Todo" plain
                      (file ,temp-capture-file)
                      ,(concat "* %(issue-todo-state) %(issue-summary) :%(issue-key):\n"
                               "%(issue-org-properties)\n"
                               "%(issue-description)"))))
                  (jirassic-org-add-attachments nil)
                  (jirassic-org-todo-state-alist '(("In Progress" . "IN PROGRESS"))))

          (aio-wait-for (jirassic-org-capture "TEST-1" nil "t"))

          (let ((captured-content (with-current-buffer (get-file-buffer temp-capture-file)
                                    (buffer-string))))
            (should (string-match-p "^\\* IN PROGRESS This is a test summary :TEST-1:" captured-content))
            (should (string-match-p ":PROPERTIES:" captured-content))
            (should (string-match-p ":issue-key: TEST-1" captured-content))
            (should (string-match-p ":issue-id: 10001" captured-content))
            (should (string-match-p ":issue-type: Task" captured-content))
            (should (string-match-p ":issue-creator: Test Creator" captured-content))
            (should (string-match-p ":issue-project: TESTPROJ" captured-content))
            (should (string-match-p ":TEMPLATE_KEYS: t" captured-content))
            (should (string-match-p ":END:" captured-content))
            (should (string-match-p "\nTest description content.\n" captured-content))))

      (when (file-exists-p temp-capture-file)
        (delete-file temp-capture-file)))))

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
          (aio-wait-for (jirassic-org-capture "TEST-1" nil "n")) ; Pass "n" as keys

          ;; Verify the content of the capture file
          (let ((captured-content (with-current-buffer (get-file-buffer temp-capture-file)
                                    (buffer-string))))
            ;; Check that the "Note" template was used
            (should (string-match-p "^\\* NOTE This is a test summary :N:" captured-content))))
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
