;;; jirassic-org.el --- Org-mode integration for Jirassic -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Emil van der Westhuizen
;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>

;;; Commentary:

;; High-level org-mode commands for capturing and displaying Jira issues.

;;; Code:
(require 'aio)
(require 'org)

(require 'jirassic-client)
(require 'jirassic-jira-parser)
(require 'jirassic-org-serializer)


(defun jirassic--build-issue-url-pattern ()
  "Build an issue URL pattern for `jirassic-host'."
  (rx-to-string
   `(seq ,(concat (string-remove-suffix "/" jirassic-host)
                  "/browse/")
         (group (+ (any upper)) "-" (+ digit)))))

(defun jirassic-capture--maybe-fetch-issue (&rest _)
  "Fetch and stash Jira issue data when capturing a Jira template."
  (when (org-capture-get :jirassic)
    (condition-case err
        (let* ((url-pattern (jirassic--build-issue-url-pattern))
               (key-or-url (read-string "Jira issue key: "))
               (key (or (save-match-data (when (string-match url-pattern key-or-url)
                                           (match-string 1 key-or-url)))
                        key-or-url))
               (issue (aio-wait-for (jirassic-get-issue key))))
          (org-capture-put :jira-issue issue)
          (org-link-add-props
           :issue-key (jira-issue-key issue)
           :issue-id (jira-issue-id issue)
           :issue-summary (jira-issue-summary issue)
           :issue-description (jirassic--serialize-to-org (jira-issue-description issue))))
      (error
       (error "Failed to fetch issue Jira issue: %s"
              (error-message-string err))))))

(advice-add 'org-capture-set-target-location :before
            #'jirassic-capture--maybe-fetch-issue)

(aio-defun jirassic-insert-issue (key &optional level)
  "Fetch Jira issue with KEY and insert at point as an org heading at LEVEL."
  (interactive "sIssue key: ")
  (let* ((issue (aio-await (jirassic-get-issue issue-key))))
    (insert (jirassic--serialize-to-org issue))))

(defun jirassic-org--build-property-drawer (props)
  "Build an org property drawer string with PROPS."
  (concat
   ":PROPERTIES:\n"
   (mapconcat (pcase-lambda (`(,prop-name ,prop-val))
                (format ":%s: %s" prop-name prop-val))
              props "\n")
   "\n:END:"))

(provide 'jirassic-org)
;;; jirassic-org.el ends here
