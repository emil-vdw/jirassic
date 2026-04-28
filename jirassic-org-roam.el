;;; jirassic-org-roam.el --- Org-roam integration for Jirassic -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Emil van der Westhuizen
;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>

;;; Commentary:

;;; Code:
(require 'org-roam)

(require 'jirassic-client)
(require 'jirassic-org)
(require 'jirassic-org-serializer)

(defcustom jirassic-org-roam-templates
  `(("i" "Issue" plain "%?"
     :target
     (file+head "${issue-key}-${issue-summary-slug}.org"
                ,(concat
                  "${issue-property-drawer}\n"
                  "#+title: ${issue-summary}\n"
                  "#+category: ${issue-summary}\n\n"
                  "${issue-description}"))
     :unnarrowed t))
  "Default org-roam Jira capture templates.")

(defun jirassic-org-roam-capture (key-or-url)
  "Capture a Jira issue form KEY-OR-URL using an Org-roam template.

ISSUE-KEY can be either a normal Jira issue key, eg. `XYZ-123',
or a full URL to the issue.

GOTO and KEYS function the same as they do in `org-roam-capture'.
TEMPLATES is a list of Org-roam templates to use for capturing,
and defaults to `jirassic-org-roam-capture-templates'.

Fetches the Jira issue and supplies a lot of extra information to the
Org-roam template. For a full list of available variables, see the
`jirassic-org-roam-capture-templates' variable."
  (interactive "sIssue key: ")
  (let* ((url-pattern (jirassic--build-issue-url-pattern))
         (key (or (save-match-data
                    (when (string-match url-pattern key-or-url)
                      (match-string 1 key-or-url)))
                  key-or-url))
         (issue (aio-wait-for (jirassic-get-issue key)))
         (node (org-roam-node-create))
         (issue-summary (jira-issue-summary issue))
         (issue-summary-slug (replace-regexp-in-string
                              "[^a-zA-Z0-9_]+" "_"
                              (downcase issue-summary)))
         (issue-property-drawer (jirassic-org--issue-properties issue
                                                                `((ROAM_ALIASES ,key)))))
    (org-roam-capture-
     :node node
     :info
     (list :issue-id (jira-issue-id issue)
           :issue-key key
           :issue-summary issue-summary
           :issue-summary-slug issue-summary-slug
           :issue-description (jirassic--serialize-to-org (jira-issue-description issue))
           :issue-property-drawer issue-property-drawer)
     :templates jirassic-org-roam-templates)))

(provide 'jirassic-org-roam)
;;; jirassic-org-roam.el ends here
