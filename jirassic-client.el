;;; jirassic-client.el --- Jira API client -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Emil van der Westhuizen
;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>

;;; Commentary:

;; Jira Rest API client.

;;; Code:
(require 'auth-source)

(require 'aio)
(require 'plz)

(defcustom jirassic-host nil
  "Jira host URL."
  :type 'string
  :group 'jirassic)

(defun jirassic-get-issue (issue-key)
  "Asynchronously fetch a Jira issue by ISSUE-KEY and resolve JSON data."
  (let ((promise (aio-promise))
        (issue-url (string-join
                    (list (jirassic--jira-api-url) "issue" issue-key) "/")))
    (plz 'get issue-url
      :headers (jirassic-client--headers (jirassic-client--credentials))
      :as #'json-read
      ;; When the request is successful, just resolve the promise with the parsed JSON data,
      :then (lambda (data) (aio-resolve promise (lambda () data)))
      ;; otherwise just signal the error.
      :else (lambda (err) (aio-resolve promise (lambda () (signal (car err) (cdr err))))))
    promise))

(defun jirassic--jira-api-url ()
  "Returns the Jira Rest API URL for JIRASSIC-HOST."
  (concat (string-remove-suffix "/" (jirassic--host)) "/rest/api/3"))

(defun jirassic-client--credentials ()
  "API credentials for JIRASSIC-HOST."
  (or (car (auth-source-search
            :host (url-host (url-generic-parse-url jirassic-host))
            :require '(:user :secret)
            :max 1))
      (error "No credentials found for host: %s" jirassic-host)))

(defun jirassic-client--headers (credentials)
  "Jira API HTTP auth headers for JIRASSIC-HOST."
  (let* ((username (plist-get credentials :user))
         (secret (plist-get credentials :secret))
         (token (if (functionp secret) (funcall secret) secret)))
    `(("Authorization" .
       ,(concat "Basic "
                (base64-encode-string
                 (concat
                  username ":" token) t))))))

(provide 'jirassic-client)
;;; jirassic-client.el ends here
