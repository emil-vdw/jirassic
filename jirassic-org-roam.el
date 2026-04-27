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
  '("i" "Issue" plain "%?"
    :target
    (file+head "${issue-key}-${issue-summary-slug}.org"
               ,(concat
                 "%(issue-org-properties `((\"ROAM_ALIASES\" . ,(issue-key))))"
                 "#+title: %(issue-summary)\n"
                 "#+category: %(issue-summary)\n\n"
                 "%(issue-description)"))))

;;; TODO: complete function
(defun jirassic-roam-capture (key-or-url)
  "Create a new Org-roam node from a Jira issue with KEY-OR-URL."
  (interactive "sIssue key: ")
  (let* ((url-pattern (jirassic--build-issue-url-pattern))
         (key (or (save-match-data (when (string-match url-pattern key-or-url)
                                     (match-string 1 key-or-url)))
                  key-or-url))
         (issue (jirassic-fetch-issue key))
         (node (org-roam-node-create
                :title (format "[%s] %s"
                               (jirassic-issue-key issue)
                               (jirassic-issue-summary issue)))))
    (org-roam-capture-
     :node node
     :info
     (list :jira-key key
           :jira-summary (jirassic-issue-summary issue)
           :jira-description (jirassic-issue-description-org issue))
     :templates jirassic-roam-capture-templates)))

(provide 'jirassic-org-roam)
;;; jirassic-org-roam.el ends here
