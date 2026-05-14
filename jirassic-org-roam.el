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
           (issue (aio-await (jirassic-get-issue key)))
           (templates (or templates jirassic-org-roam-capture-templates))
           ;; Resolve the template key up-front so we can record it on
           ;; the captured node for later use by `jirassic-org-roam-pull'.
           (template-key
            (or keys
                (car (let ((org-capture-templates templates))
                       (org-capture-select-template)))))
           (extra-drawer-props
            `((ROAM_ALIASES ,key)
              ,@(when jirassic-org-store-template-key
                  `(("issue-template-key" ,template-key))))))
      (org-roam-capture-
       :goto goto
       :keys template-key
       :node (or node (org-roam-node-create))
       :info (seq-concatenate 'list
                              (jirassic-org--issue-properties issue
                                                              extra-drawer-props)
                              info)
       :props props
       :templates templates))))

;;;###autoload
(defun jirassic-org-roam-pull ()
  "Pull the latest Jira issue for the Org-roam node at point and ediff it locally.

The issue is identified by the `issue-key' property of the node at
point. The remote issue is rendered using the selected template from
`jirassic-org-roam-capture-templates' (referenced by the
`issue-template-key' property, with a fallback prompt) and compared
against the node's content using `ediff'.

Only `entry' and `plain' capture template types are supported, and the
template body must be a literal string."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (let* ((node (or (org-roam-node-at-point)
                   (user-error "Must be used in an org-roam node")))
         (issue-key (or (org-entry-get nil "issue-key")
                        (user-error "No issue-key property on this entry")))
         (org-capture-templates jirassic-org-roam-capture-templates)
         (template-entry (condition-case nil
                             ;; Try to use the stored template key in
                             ;; the org property drawer.
                             (org-capture-select-template
                              (org-entry-get nil "issue-template-key"))
                           ;; If that doesn't match any template
                           ;; anymore, prompt the user to select one
                           ;; normally.
                           (error (org-capture-select-template))))
         (template-key (car template-entry))
         (entry-type (nth 2 template-entry))
         (template (nth 3 template-entry))
         (target (plist-get (nthcdr 4 template-entry) :target))
         (target-type (car-safe target))
         (head (and (memq target-type '(file+head file+head+olp))
                    (nth 2 target))))
    (unless (memq entry-type '(entry plain))
      (user-error
       "Unsupported capture template type `%s'; `jirassic-org-roam-pull' only supports `entry' and `plain'"
       entry-type))
    (unless (stringp template)
      (user-error
       "Unsupported capture template body; `jirassic-org-roam-pull' requires a literal string template"))
    (when (and (eq entry-type 'entry)
               (not (org-current-level)))
      (user-error "Point must be on or under a heading for `entry' templates"))
    (let* ((source-buffer (current-buffer))
           (source-entry-level (when (eq entry-type 'entry)
                                 (org-current-level)))
           (source-entry-start (when (eq entry-type 'entry)
                                 (save-excursion
                                   (org-back-to-heading t) (point))))
           (extra-drawer-props
            `((ROAM_ALIASES ,issue-key)
              (ID ,(org-roam-node-id node))
              ,@(when jirassic-org-store-template-key
                  `(("issue-template-key" ,template-key)))))
           (issue (aio-wait-for (jirassic-get-issue issue-key)))
           (source-indirect (make-indirect-buffer source-buffer
                                                  (format "*%s-current*" issue-key)
                                                  t))
           (pull-buffer (generate-new-buffer (format "*%s-latest*" issue-key)))
           ;; Track whether setup and handover to ediff was successful.
           (ediff-handover nil))
      (unwind-protect
          (progn
            (with-current-buffer pull-buffer
              (org-mode)
              (let ((org-roam-capture--node node)
                    (org-roam-capture--info
                     (jirassic-org--issue-properties issue extra-drawer-props)))
                ;; Unless capturing to an entry, diff the whole file,
                ;; including the head because it is probably capturing
                ;; at the file level.
                (when (and (not (eq entry-type 'entry)) head)
                  (insert (org-roam-capture--fill-template head)))
                ;; Insert but keep the point before the inserted text
                ;; because we might need to manipulate the entry to
                ;; match the level it's at in the source buffer.
                (save-excursion (insert (org-roam-capture--fill-template template)))
                (when (and (eq entry-type 'entry) (org-at-heading-p))
                  (let ((delta (- source-entry-level (org-current-level))))
                    (cond ((> delta 0) (dotimes (_ delta) (org-demote-subtree)))
                          ((< delta 0) (dotimes (_ (- delta)) (org-promote-subtree)))))))
              (set-buffer-modified-p nil))

            (when (eq entry-type 'entry)
              (with-current-buffer source-indirect
                (goto-char source-entry-start)
                (org-narrow-to-subtree)))

            (jirassic-org--pull-ediff source-indirect pull-buffer)
            (setq ediff-handover t))

        (unless ediff-handover
          (when (buffer-live-p pull-buffer) (kill-buffer pull-buffer))
          (when (buffer-live-p source-indirect) (kill-buffer source-indirect)))))))

(provide 'jirassic-org-roam)
;;; jirassic-org-roam.el ends here
