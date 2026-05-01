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


(defcustom jirassic-jira-to-org-keyword-alist nil
  "An alist mapping Jira status strings to Org TODO keyword strings."
  :type '(alist :key-type string :value-type string)
  :group 'jirassic)

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
          (apply #'org-link-add-props (jirassic-org--issue-properties issue)))
      (error
       (error "Failed to fetch issue Jira issue: %s"
              (error-message-string err))))))

(defun jirassic-org-capture (key-or-url &optional goto keys)
  "Org capture from a Jira issue from KEY-OR-URL."
  (interactive "sIssue Key: ")
  (let* ((url-pattern (jirassic--build-issue-url-pattern))
         (key (or (save-match-data
                    (when (string-match url-pattern key-or-url)
                      (match-string 1 key-or-url)))
                  key-or-url))
         (issue (aio-wait-for (jirassic-get-issue key))))
    (apply #'org-link-add-props (jirassic-org--issue-properties issue))
    (let ((org-capture-link-is-already-stored t))
      (org-capture goto keys))))

(aio-defun jirassic-insert-issue (key &optional level)
  "Fetch Jira issue with KEY and insert at point as an org heading at LEVEL."
  (interactive "sIssue key: ")
  (let* ((issue (aio-await (jirassic-get-issue issue-key))))
    (insert (jirassic--serialize-to-org issue))))

(defun jirassic-org--issue-property-drawer (issue &optional extra-props)
  "Return an org property drawer for Jira ISSUE.

EXTRA-PROPS can be an alist of extra properties to include in the drawer."
  (let ((issue-props `(("issue-id" ,(jira-issue-id issue))
                       ("issue-key" ,(jira-issue-key issue)))))
    (concat ":PROPERTIES:\n"
            (mapconcat (lambda (prop)
                         (format ":%s: %s" (car prop) (cadr prop)))
                       (seq-concatenate 'list issue-props extra-props)
                       "\n")
            "\n:END:")))

(defun jirassic-org--issue-properties (issue)
  "Return a plist of ISSUE props for template substitution."
  (let* ((issue-summary (jira-issue-summary issue))
         (issue-key (jira-issue-key issue))
         (issue-summary-slug (replace-regexp-in-string
                              "[^a-zA-Z0-9_]+" "_"
                              (downcase issue-summary)))
         (issue-property-drawer (jirassic-org--issue-property-drawer issue
                                                                     `((ROAM_ALIASES ,issue-key))))
         (issue-status (jira-issue-status issue)))
    (list :issue-id (jira-issue-id issue)
          :issue-status issue-status
          :issue-todo-keyword (alist-get issue-status
                                         jirassic-jira-to-org-keyword-alist
                                         issue-status)
          :issue-type (jira-issue-type issue)
          :issue-key issue-key
          :issue-summary issue-summary
          :issue-summary-slug issue-summary-slug
          :issue-description (jirassic--serialize-to-org
                              (jirassic-adjust-heading-level
                               (jira-issue-description issue) 1))
          :issue-property-drawer issue-property-drawer)))

(provide 'jirassic-org)
;;; jirassic-org.el ends here
