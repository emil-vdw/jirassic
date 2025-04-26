;;; jirassic-org-roam.el --- Jirassic Org-roam integration -*- lexical-binding: t; -*-

;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>
;; Maintainer: Emil van der Westhuizen <vdwemil@protonmail.com>
;; Created: 24 April 2025
;; Version: 0.1
;; Package-Requires: ((emacs "29.3") (jirassic "0.1") (org-roam "2.0") (aio "1.0.0"))
;; Homepage: https://github.com/emil-vdw/jirassic
;; Keywords:

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides Org-roam integration for Jirassic, allowing you to capture
;; Jira issues as Org-roam notes.

;;; Code:
(require 'aio)
(require 'org-roam)

(require 'jirassic-client)
(require 'jirassic-core)
(require 'jirassic-issue)
(require 'jirassic-serializer)

(defcustom jirassic-roam-capture-templates
  `(("i" "Issue" plain "%?"
     :target
     (file+head "${issue-key}-${issue-summary-slug}.org"
                ,(mapconcat #'identity
                            (list
                             ":PROPERTIES:"
                             ":issue-key: ${issue-key}"
                             ":issue-link: ${issue-link}"
                             ":issue-id: ${issue-id}"
                             ":issue-type: ${issue-type}"
                             ":issue-creator: ${issue-creator-name}"
                             ":issue-project: ${issue-project}"
                             ":ROAM_ALIASES: ${issue-key}"
                             ":END:"
                             "#+title: ${issue-summary}"
                             "#+category: ${issue-summary}\n"
                             "${issue-description}")
                            "\n"))
     :unnarrowed t))
  "Org roam capture templates for Jira issues.

The following variables are
available in the template:
- `issue-key': The issue key, e.g. 'XYZ-123'.
- `issue-id'
- `issue-description': The issue description, formatted as a
  string.
- `issue-summary'
- `issue-summary-slug': A slug of the issue summary.
- `issue-type'
- `issue-priority'
- `issue-status'
- `issue-creator-name'
- `issue-creator-email'
- `issue-project'
- `issue-link'")

;;;###autoload
(aio-defun jirassic-org-roam-capture (issue-key &optional goto keys)
  "Capture a Jira issue with Org-roam templates.

ISSUE-KEY can be either a normal Jira issue key, eg. 'XYZ-123',
or a full URL to the issue.

GOTO and KEYS function the same as they do in `org-roam-capture'.

This function fetches the Jira issue and supplies a lot of extra
information to the Org-roam template. For a full list of available
variables, see the `jirassic-roam-capture-templates' variable."
  (interactive "sIssue key: ")
  (condition-case err
      (let* ((issue-key (if (jirassic-issue-link-p issue-key)
                            (jirassic-issue-key-from-link issue-key)
                          issue-key))
             (issue (aio-await (jirassic-get-issue issue-key)))
             (issue-description
              (jirassic--doc-string (jirassic-issue-description issue) 1))
             (issue-info
              (list :issue-key issue-key
                    :issue-id (jirassic-issue-id issue)
                    :issue-description issue-description
                    :issue-summary (jirassic-issue-summary issue)
                    :issue-summary-slug (replace-regexp-in-string
                                         "[^a-z0-9]+"
                                         "_"
                                         (downcase (jirassic-issue-summary issue)))
                    :issue-type (jirassic-issue-type issue)
                    :issue-priority (jirassic-issue-priority issue)
                    :issue-status (jirassic-issue-status issue)
                    :issue-creator-name (jirassic-user-display-name (jirassic-issue-creator issue))
                    :issue-creator-email (jirassic-user-display-name (jirassic-issue-creator issue))
                    :issue-project (jirassic-issue-project issue)
                    :issue-link (jirassic-issue-link issue))))

        (org-roam-capture- :goto (when goto '(4))
                           :info issue-info
                           :keys keys
                           :node (org-roam-node-create)
                           :templates jirassic-roam-capture-templates))

    (jirassic-client-error
     (message "Error fetching issue '%s': %s"
              issue-key
              (jirassic-http-error-message (cdr err))))
    (error
     (message "Error capturing issue %s: %s"
              issue-key
              (error-message-string err)))))

(provide 'jirassic-org-roam)
;;; jirassic-org-roam.el ends here
