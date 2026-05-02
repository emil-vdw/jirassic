;;; jirassic-client-test.el --- Tests for jirassic-client -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'ert)
(require 'plz)

(require 'jirassic-client)

;;; `jirassic--jira-api-url'

(ert-deftest jirassic-client-test-api-url ()
  (let ((jirassic-host "https://acme.atlassian.net"))
    (should (string= (jirassic--jira-api-url)
                     "https://acme.atlassian.net/rest/api/3")))

  ;; Trailing slash is stripped.
  (let ((jirassic-host "https://acme.atlassian.net/"))
    (should (string= (jirassic--jira-api-url)
                     "https://acme.atlassian.net/rest/api/3")))

  ;; nil host signals jirassic-error before making any network call.
  (let ((jirassic-host nil))
    (should-error (jirassic--jira-api-url) :type 'jirassic-error)))

;;; `jirassic-client--headers'

(ert-deftest jirassic-client-test-headers-string-secret ()
  (let* ((credentials '(:user "user@example.com" :secret "mytoken"))
         (expected (concat "Basic " (base64-encode-string "user@example.com:mytoken" t)))
         (headers (jirassic-client--headers credentials)))
    (should (equal headers `(("Authorization" . ,expected))))))

(ert-deftest jirassic-client-test-headers-function-secret ()
  ;; auth-source returns the secret as a callable — it should be invoked.
  (let* ((credentials `(:user "user@example.com" :secret ,(lambda () "mytoken")))
         (expected (concat "Basic " (base64-encode-string "user@example.com:mytoken" t)))
         (headers (jirassic-client--headers credentials)))
    (should (equal headers `(("Authorization" . ,expected))))))

;;; `jirassic-client--http-error-data'

(ert-deftest jirassic-client-test-http-error-data-http-error ()
  ;; HTTP errors have a response with a status code.
  (let* ((plz-err (make-plz-error
                   :response (make-plz-response :status 404)
                   :message "Issue does not exist"))
         (data (jirassic-client--http-error-data plz-err)))
    (should (= (car data) 404))
    (should (string= (cadr data) "Issue does not exist"))))

(ert-deftest jirassic-client-test-http-error-data-curl-error ()
  ;; Curl errors have no response but carry a (code . message) curl-error.
  (let* ((plz-err (make-plz-error
                   :curl-error '(6 . "Could not resolve host")))
         (data (jirassic-client--http-error-data plz-err)))
    (should (eq (car data) nil))
    (should (string= (cadr data) "curl error 6: Could not resolve host"))))

;;; `jirassic-get-issue'

(defvar jirassic-client-test--mock-issue-data
  '((self . "https://acme.atlassian.net/rest/api/3/issue/10001")
    (id . "10001")
    (key . "TEST-1")
    (fields
     (summary . "Test issue")
     (status (name . "In Progress"))
     (issuetype (name . "Task"))
     (priority (name . "Medium"))
     (description (type . "doc") (content . [])))))

(defmacro jirassic-client-test--with-mock-plz (handler &rest body)
  "Evaluate BODY with `plz' replaced by HANDLER.

HANDLER is a lambda that receives (METHOD URL &rest ARGS) and
should call either the :then or :else keyword argument."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'plz) ,handler)
             ((symbol-function 'jirassic-client--credentials)
              (lambda () '(:user "user@example.com" :secret "token"))))
     ,@body))

(ert-deftest jirassic-client-test-get-issue-success ()
  (let ((jirassic-host "https://acme.atlassian.net"))
    (jirassic-client-test--with-mock-plz
        (lambda (_method _url &rest args)
          (funcall (plist-get args :then) jirassic-client-test--mock-issue-data))
      (let ((issue (aio-wait-for (jirassic-get-issue "TEST-1"))))
        (should (cl-typep issue 'jira-issue))
        (should (string= (jira-issue-key issue) "TEST-1"))
        (should (string= (jira-issue-summary issue) "Test issue"))
        (should (string= (jira-issue-url issue)
                         "https://acme.atlassian.net/browse/TEST-1"))))))

(ert-deftest jirassic-client-test-get-issue-http-error ()
  (let ((jirassic-host "https://acme.atlassian.net"))
    (jirassic-client-test--with-mock-plz
        (lambda (_method _url &rest args)
          (funcall (plist-get args :else)
                   (make-plz-error
                    :response (make-plz-response :status 404)
                    :message "Issue does not exist")))
      (should-error (aio-wait-for (jirassic-get-issue "NOTFOUND-1"))
                    :type 'jirassic-http-error)
      (condition-case err
          (aio-wait-for (jirassic-get-issue "NOTFOUND-1"))
        (jirassic-http-error
         (should (= (jirassic-http-error-code err) 404))
         (should (string= (jirassic-http-error-message err)
                          "Issue does not exist")))))))

(ert-deftest jirassic-client-test-get-issue-no-host ()
  ;; Missing host is caught before any network call and signals `jirassic-error'.
  (let ((jirassic-host nil))
    (should-error (aio-wait-for (jirassic-get-issue "TEST-1"))
                  :type 'jirassic-error)))

(ert-deftest jirassic-client-test-get-issue-no-credentials ()
  ;; Missing credentials signal `jirassic-error'.
  (let ((jirassic-host "https://acme.atlassian.net"))
    (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _) nil)))
      (should-error (aio-wait-for (jirassic-get-issue "TEST-1"))
                    :type 'jirassic-error))))

(provide 'jirassic-client-test)
;;; jirassic-client-test.el ends here
