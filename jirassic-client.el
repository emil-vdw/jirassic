;;; jirassic-client.el --- Jira API client -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Emil van der Westhuizen
;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>

;;; Commentary:

;; Jira Rest API client.

;;; Code:
(require 'auth-source)

(require 'aio)
(require 'plz)
(require 'jirassic-jira-parser)

(define-error 'jirassic-http-error "Jira HTTP error" 'jirassic-error)

(defcustom jirassic-host nil
  "Jira host URL."
  :type 'string
  :group 'jirassic)

(defun jirassic-http-error-code (err)
  "Return the HTTP status code from a caught `jirassic-http-error' ERR, or nil."
  (cadr err))

(defun jirassic-http-error-message (err)
  "Return the message string from a caught `jirassic-http-error' ERR."
  (caddr err))

(defun jirassic-get-issue (issue-key)
  "Asynchronously fetch Jira issue with ISSUE-KEY, resolving a `jira-issue' struct.

Signals `jirassic-http-error' on HTTP or network failure and
`jirassic-error' for configuration problems (missing host or credentials)."
  (let ((promise (aio-promise)))
    (condition-case err
        (let ((issue-url (string-join
                          (list (jirassic--jira-api-url) "issue" issue-key) "/")))
          (plz 'get issue-url
            :headers (jirassic-client--headers (jirassic-client--credentials))
            :as #'json-read
            :then (lambda (data)
                    (aio-resolve promise (lambda () (jirassic--parse-issue data))))
            :else (lambda (plz-err)
                    (aio-resolve promise
                                 (lambda ()
                                   (signal 'jirassic-http-error
                                           (jirassic-client--http-error-data plz-err)))))))
      (error
       (aio-resolve promise (lambda () (signal (car err) (cdr err))))))
    promise))

(defun jirassic-client--http-error-data (plz-err)
  "Extract (CODE MESSAGE) from PLZ-ERR for `jirassic-http-error' signal data."
  (let* ((response (plz-error-response plz-err))
         (code (when response (plz-response-status response)))
         (message (or (plz-error-message plz-err)
                      (when-let* ((curl-err (plz-error-curl-error plz-err)))
                        (format "curl error %d: %s" (car curl-err) (cdr curl-err))))))
    (list code message)))

(defun jirassic--jira-api-url ()
  "Return the Jira Rest API URL."
  (unless jirassic-host
    (signal 'jirassic-error '("jirassic-host is not configured")))
  (concat (string-remove-suffix "/" jirassic-host) "/rest/api/3"))

(defun jirassic-client--credentials ()
  "API credentials for `jirassic-host'."
  (or (car (auth-source-search
            :host (url-host (url-generic-parse-url jirassic-host))
            :require '(:user :secret)
            :max 1))
      (signal 'jirassic-error
              (list (format "No credentials found for host: %s" jirassic-host)))))

(defun jirassic-client--headers (credentials)
  "Build Jira API authorization headers from CREDENTIALS."
  (let* ((username (plist-get credentials :user))
         (secret (plist-get credentials :secret))
         (token (if (functionp secret) (funcall secret) secret)))
    `(("Authorization" .
       ,(concat "Basic "
                (base64-encode-string (concat username ":" token) t))))))

(provide 'jirassic-client)
;;; jirassic-client.el ends here
