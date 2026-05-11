;;; jirassic-org.el --- Org-mode integration for Jirassic -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Emil van der Westhuizen
;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>

;;; Commentary:

;; High-level org-mode commands for capturing and displaying Jira issues.

;;; Code:
(require 'aio)
(require 'ediff)
(require 'org)
(require 'org-capture)

(require 'jirassic-client)
(require 'jirassic-jira-parser)
(require 'jirassic-org-serializer)


(defcustom jirassic-jira-to-org-keyword-alist nil
  "An alist mapping Jira status strings to Org TODO keyword strings."
  :type '(alist :key-type string :value-type string)
  :group 'jirassic)

(defvar org-capture-link-is-already-stored)

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
    (jirassic-org--with-capture-context issue
      (org-capture goto keys))))

(defmacro jirassic-org--with-capture-context (issue &rest body)
  "Set up org capture context for ISSUE, then evaluate BODY.

Stores the issue link properties for template substitution, binds
`jirassic-current-issue' so that sexps in capture templates can
access the full issue struct, and prevents `org-capture' from
calling `org-store-link' and overwriting those properties."
  (declare (indent 1)
           (debug (form body)))
  (let ((issue-var (make-symbol "issue")))
    `(let ((,issue-var ,issue))
       (apply #'org-link-store-props (jirassic-org--issue-properties ,issue-var))
       (let ((jirassic-current-issue ,issue-var)
             (org-capture-link-is-already-stored t))
         ,@body))))

(aio-defun jirassic-insert-issue (key &optional _level)
  "Fetch Jira issue with KEY and insert at point as an org heading at LEVEL."
  (interactive "sIssue key: ")
  (let* ((issue (aio-await (jirassic-get-issue key))))
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
         (creator-props (when-let* ((creator (jira-issue-creator issue)))
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

EXTRA-DRAWER-PROPS is an alist of extra props to include in the
formatted org property drawer."
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

(defun jirassic-org--ediff-pull-buffers (source-buffer pull-buffer &optional extra-kill-buffers)
  "Ediff SOURCE-BUFFER against PULL-BUFFER, restoring window state on quit.

When the user quits ediff, PULL-BUFFER and any buffers in EXTRA-KILL-BUFFERS
are killed, and the window configuration captured at call time is restored."
  (let ((window-config (current-window-configuration))
        (kill-buffers (cons pull-buffer extra-kill-buffers)))
    (ediff-buffers source-buffer pull-buffer
                   (list (lambda ()
                           (add-hook 'ediff-cleanup-hook
                                     (lambda ()
                                       (dolist (buf kill-buffers)
                                         (when (buffer-live-p buf)
                                           (kill-buffer buf)))
                                       (set-window-configuration window-config))
                                     nil t))))))

(defun jirassic-org-pull ()
  "Pull the latest version of the Jira issue at point and ediff it locally.

The issue is identified by the `issue-key' property on the current entry.
The remote issue is rendered with the org capture template referenced by
the `issue-template-key' property, falling back to an interactive template
prompt when that key no longer resolves.  The rendered result is then
compared against the current subtree using `ediff'."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (let* ((source-buffer (current-buffer))
         (source-entry-start (save-excursion (org-back-to-heading t) (point)))
         (source-entry-level (org-current-level))
         (issue-key (or (org-entry-get nil "issue-key")
                        (user-error "No issue-key property on this heading")))
         (template-entry (condition-case nil
                             ;; Try to use the stored template key in
                             ;; the org property drawer.
                             (org-capture-select-template
                              (org-entry-get nil "issue-template-key"))
                           ;; If that doesn't match any template
                           ;; anymore, prompt the user to select one
                           ;; normally.
                           (error (org-capture-select-template))))
         (template-string (nth 4 template-entry))
         (issue (aio-wait-for (jirassic-get-issue issue-key)))
         (pull-buffer (generate-new-buffer (format "*%s-latest*" issue-key)))
         (source-indirect nil)
         ;; Track whether setup and handover to ediff was successful.
         (ediff-handover nil))
    (unwind-protect
        (progn
          (with-current-buffer pull-buffer
            (org-mode)
            (jirassic-org--with-capture-context issue
              (let ((org-capture-plist (list :template template-string
                                             :buffer pull-buffer)))
                (insert (string-replace "%?" "" (org-capture-fill-template)))))
            (goto-char (point-min))
            ;; Make sure that the both entries are at the same level
            (when (org-at-heading-p)
              (let ((delta (- source-entry-level (org-current-level))))
                (cond ((> delta 0) (dotimes (_ delta) (org-demote-subtree)))
                      ((< delta 0) (dotimes (_ (- delta)) (org-promote-subtree))))))
            (set-buffer-modified-p nil))
          (setq source-indirect
                (make-indirect-buffer source-buffer
                                      (format "*%s-current*" issue-key)
                                      t))
          (with-current-buffer source-indirect
            (goto-char source-entry-start)
            (org-narrow-to-subtree))
          (jirassic-org--ediff-pull-buffers source-indirect pull-buffer
                                            (list source-indirect))
          (setq ediff-handover t))
      (unless ediff-handover
        (when (buffer-live-p pull-buffer) (kill-buffer pull-buffer))
        (when (buffer-live-p source-indirect) (kill-buffer source-indirect))))))

(provide 'jirassic-org)
;;; jirassic-org.el ends here
