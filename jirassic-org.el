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

(defvar jirassic-current-issue nil
  "The `jira-issue' currently being captured.

Bound dynamically during `jirassic-org-capture' so capture template
sexps can access the full issue struct via this variable.")

(defun jirassic--build-issue-url-pattern ()
  "Build an issue URL pattern for `jirassic-host'."
  (rx-to-string
   `(seq ,(concat (string-remove-suffix "/" jirassic-host)
                  "/browse/")
         (group (+ (any upper)) "-" (+ digit)))))

(defun jirassic-org-capture (key-or-url &optional goto keys)
  "Org capture from a Jira issue from KEY-OR-URL.

GOTO and KEYS are passed to `org-capture' directly."
  (interactive "sIssue Key: ")
  (let* ((url-pattern (jirassic--build-issue-url-pattern))
         (key (or (save-match-data
                    (when (string-match url-pattern key-or-url)
                      (match-string 1 key-or-url)))
                  key-or-url))
         (issue (aio-wait-for (jirassic-get-issue key))))
    (apply #'org-link-store-props (jirassic-org--issue-properties issue))
    (let (;; Set `jirassic-current-issue' so that this can be used by
          ;; sexps in the template.
          (jirassic-current-issue issue)
          ;; Prevent `org-capture' from invoking `org-store-link' and
          ;; overwriting the link props we just stored.
          (org-capture-link-is-already-stored t))
      (org-capture goto keys))))

(aio-defun jirassic-insert-issue (key &optional level)
  "Fetch Jira issue with KEY and insert at point as an org heading at LEVEL."
  (interactive "sIssue key: ")
  (let* ((issue (aio-await (jirassic-get-issue issue-key))))
    (insert (jirassic--serialize-to-org issue))))

(defun jirassic-org--issue-property-drawer (issue &optional extra-props)
  "Return an org property drawer for Jira ISSUE.

EXTRA-PROPS can be an alist of extra properties to include in the drawer."
  (let* ((issue-props `(("issue-key" ,(jira-issue-key issue))
                        ("issue-id" ,(jira-issue-id issue))
                        ("issue-url" ,(jira-issue-url issue))
                        ("issue-type" ,(jira-issue-type issue))
                        ("issue-priority" ,(jira-issue-priority issue))
                        ("issue-project-key" ,(jira-project-key (jira-issue-project issue)))
                        ("issue-project-name" ,(jira-project-name (jira-issue-project issue)))))
         (creator-props (when-let (creator (jira-issue-creator issue))
                          `(("issue-creator-email" ,(jira-user-email creator))
                            ("issue-creator-display-name" ,(jira-user-display-name creator))))))
    (concat ":PROPERTIES:\n"
            (mapconcat (lambda (prop)
                         (format ":%s: %s" (car prop) (cadr prop)))
                       (seq-concatenate 'list issue-props creator-props extra-props)
                       "\n")
            "\n:END:")))

(defun jirassic-org--issue-properties (issue &optional extra-drawer-props)
  "Return a plist of ISSUE props for template substitution.

EXTRA-DRAWER-PROPS is an alist of extra props to include in the formatted org drawer."
  (let* ((issue-summary (jira-issue-summary issue))
         (issue-key (jira-issue-key issue))
         (issue-summary-slug (replace-regexp-in-string
                              "[^a-zA-Z0-9_]+" "_"
                              (downcase issue-summary)))
         (issue-property-drawer (jirassic-org--issue-property-drawer issue
                                                                     extra-drawer-props))
         (issue-status (jira-issue-status issue))
         (creator (jira-issue-creator issue))
         (project (jira-issue-project issue)))
    (list :type "jira"
          :link (jira-issue-url issue)
          :description (jira-issue-summary issue)
          :annotation (org-link-make-string (jira-issue-url issue)
                                       (jira-issue-key issue))
          :issue-id (jira-issue-id issue)
          :issue-status issue-status
          :issue-todo-keyword (alist-get issue-status
                                         jirassic-jira-to-org-keyword-alist
                                         issue-status)
          :issue-type (jira-issue-type issue)
          :issue-priority (jira-issue-priority issue)
          :issue-creator-display-name (when creator (jira-user-display-name creator))
          :issue-creator-email (when creator (jira-user-email creator))
          :issue-project-id (when project (jira-project-id project))
          :issue-project-key (when project (jira-project-key project))
          :issue-project-name (when project (jira-project-name project))
          :issue-url (jira-issue-url issue)
          :issue-key issue-key
          :issue-summary issue-summary
          :issue-summary-slug issue-summary-slug
          :issue-description (jirassic--serialize-to-org
                              (jirassic-adjust-heading-level
                               (jira-issue-description issue) 1))
          :issue-property-drawer issue-property-drawer)))

(provide 'jirassic-org)
;;; jirassic-org.el ends here
