;;; jirassic-org-roam.el --- Org-roam integration for Jirassic -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Emil van der Westhuizen
;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>

;;; Commentary:

;;; Code:
(require 'org-roam)
(require 'org-roam-capture)

(require 'jirassic-client)
(require 'jirassic-org)
(require 'jirassic-org-serializer)

(defcustom jirassic-org-roam-capture-templates
  `(("i" "Issue" plain "%?"
     :target
     (file+head "${issue-key}-${issue-summary-slug}.org"
                ,(concat
                  "${issue-property-drawer}\n"
                  "#+title: ${issue-summary}\n"
                  "#+category: ${issue-summary}\n\n"
                  "${issue-description}"))
     :unnarrowed t))
  "Default org-roam Jira capture templates."
  :type '(repeat sexp)
  :group 'jirassic)

(cl-defun jirassic-org-roam-capture (key-or-url &key goto keys node info props templates)
  "Capture a Jira issue form KEY-OR-URL using an Org-roam template.

ISSUE-KEY can be either a normal Jira issue key, eg. `XYZ-123',
or a full URL to the issue.

GOTO, KEYS, NODE, INFO, and PROPS function the same as they do in
`org-roam-capture'.
TEMPLATES is a list of Org-roam templates to use for capturing,
and defaults to `jirassic-org-roam-capture-templates'.

Fetches the Jira issue and supplies a lot of extra information to the
Org-roam template. For a full list of available variables, see the
`jirassic-org-roam-capture-templates' variable."
  (interactive "sIssue key: ")
  (aio-with-async
    (let* ((url-pattern (jirassic--build-issue-url-pattern))
           (key (or (save-match-data
                      (when (string-match url-pattern key-or-url)
                        (match-string 1 key-or-url)))
                    key-or-url))
           (issue (aio-await (jirassic-get-issue key))))
      (org-roam-capture-
       :goto goto
       :keys keys
       :node (or node (org-roam-node-create))
       :info (seq-concatenate 'list
                              (jirassic-org--issue-properties issue
                                                              `((ROAM_ALIASES ,key)))
                              info)
       :props props
       :templates (or templates
                      jirassic-org-roam-capture-templates)))))

(defun jirassic-org-roam-pull ()
  "Pull the latest Jira issue for the Org-roam node at point and ediff it locally.

The issue is identified by the `issue-key' property of the node at point.
The remote issue is rendered using the head of the selected template from
`jirassic-org-roam-capture-templates' (with the node and issue properties
bound for `${var}' substitution), and the result is compared against the
node's file using `ediff'."
  (interactive)
  (let* ((node (org-roam-node-at-point 'assert))
         (node-file (or (org-roam-node-file node)
                        (user-error "Node has no associated file")))
         (issue-key (or (cdr (assoc-string "issue-key"
                                           (org-roam-node-properties node)
                                           t))
                        (user-error "No issue-key property on this node")))
         (template-entry (let ((org-capture-templates
                                jirassic-org-roam-capture-templates))
                           (org-capture-select-template)))
         (target (plist-get (nthcdr 4 template-entry) :target))
         (head (or (and (eq (car-safe target) 'file+head)
                        (caddr target))
                   (user-error "Selected template has no file+head target")))
         (issue (aio-wait-for (jirassic-get-issue issue-key)))
         (source-buffer-existed (find-buffer-visiting node-file))
         (source-buffer (find-file-noselect node-file))
         (pull-buffer (generate-new-buffer (format "*%s-latest*" issue-key)))
         ;; Track whether setup and handover to ediff was successful.
         (ediff-handover nil))
    (unwind-protect
        (progn
          (with-current-buffer pull-buffer
            (org-mode)
            (let ((org-roam-capture--node node)
                  (org-roam-capture--info
                   (jirassic-org--issue-properties
                    issue
                    `((ROAM_ALIASES ,issue-key)
                      (ID ,(org-roam-node-id node))))))
              (insert (org-roam-capture--fill-template head 'ensure-newline)))
            (set-buffer-modified-p nil))
          (jirassic-org--ediff-pull-buffers source-buffer pull-buffer
                                            (unless source-buffer-existed
                                              (list source-buffer)))
          (setq ediff-handover t))
      (unless ediff-handover
        (when (buffer-live-p pull-buffer) (kill-buffer pull-buffer))
        (unless source-buffer-existed
          (when (buffer-live-p source-buffer) (kill-buffer source-buffer)))))))

(provide 'jirassic-org-roam)
;;; jirassic-org-roam.el ends here
