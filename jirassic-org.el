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


(defcustom jirassic-org-capture-templates
  '(("j" "Jira Issue" entry
     (file org-default-notes-file)
     "* %:issue-todo-keyword %:issue-summary\n%:issue-property-drawer\n\n%:issue-description%?"
     :empty-lines 1
     :jump-to-captured t))
  "Org capture templates for Jira issues.

These templates are used exclusively by `jirassic-org-capture' and
`jirassic-org-pull'. These templates support substitution of Jira issue
context.

Besides the extra substitution vars, all of the standard
`org-capture-templates' features apply.

Available `%:' substitutions:

  %:annotation            Org link to the issue (key as description).
  %:issue-description     Issue body, serialized to Org.
  %:issue-id              Internal Jira ID.
  %:issue-key             Issue key, e.g. \"XYZ-123\".
  %:issue-priority        Issue priority.
  %:issue-project-key     Jira project key.
  %:issue-project-name    Jira project name.
  %:issue-property-drawer Org :PROPERTIES: drawer.
  %:issue-status          Raw Jira status string.
  %:issue-summary         Issue title.
  %:issue-summary-slug    URL-safe slug of the summary.
  %:issue-todo-keyword    Org TODO keyword (see `jirassic-jira-to-org-keyword-alist').
  %:issue-type            Issue type (Bug, Story, etc.).
  %:issue-url             URL of the issue."
  :type (get 'org-capture-templates 'custom-type)
  :set (lambda (s v) (set-default-toplevel-value s (org-capture-upgrade-templates v)))
  :group 'jirassic)

(defcustom jirassic-jira-to-org-keyword-alist nil
  "An alist mapping Jira status strings to Org TODO keyword strings."
  :type '(alist :key-type string :value-type string)
  :group 'jirassic)

(defcustom jirassic-org-store-template-key t
  "Whether to record the capture template key on captured Jira issues.

When non-nil, `jirassic-org-capture' and `jirassic-org-roam-capture'
will include an `issue-template-key' property in the rendered property
drawer. `jirassic-org-pull' and `jirassic-org-roam-pull' then uses this
property to re-render the issue with the same template, instead of
prompting for one."
  :type 'boolean
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
         (issue (aio-wait-for (jirassic-get-issue key)))
         (org-capture-templates jirassic-org-capture-templates)
         ;; Resolve the template up-front so we can record its key on
         ;; the captured entry for later use by `jirassic-org-pull' and
         ;; pick the right heading-adjust amount for its type.
         (template-entry (or (and keys (assoc keys org-capture-templates))
                             (org-capture-select-template)))
         (template-key (car template-entry))
         (template-type (nth 2 template-entry))
         (description-level-adjust (if (eq template-type 'entry) 1 0))
         (extra-drawer-props
          (when jirassic-org-store-template-key
            `(("issue-template-key" ,template-key)))))
    (jirassic-org--with-capture-context issue
        extra-drawer-props
        description-level-adjust
      (org-capture goto template-key))))

(defmacro jirassic-org--with-capture-context (issue extra-drawer-props description-level-adjust &rest body)
  "Set up org capture context for ISSUE, then evaluate BODY.

Stores the issue link properties for template substitution, binds
`jirassic-current-issue' so that sexps in capture templates can
access the full issue struct, and prevents `org-capture' from
calling `org-store-link' and overwriting those properties.

EXTRA-DRAWER-PROPS is an alist of extra properties to splice into
the rendered `%:issue-property-drawer' substitution.

DESCRIPTION-LEVEL-ADJUST controls how much the description's headings
are promoted by."
  (declare (indent 3)
           (debug (form form form body)))
  (let ((issue-var (make-symbol "issue"))
        (props-var (make-symbol "extra-drawer-props"))
        (adjust-var (make-symbol "description-level-adjust")))
    `(let* ((,issue-var ,issue)
            (,props-var ,extra-drawer-props)
            (,adjust-var ,description-level-adjust))
       (apply #'org-link-store-props
              (jirassic-org--issue-properties ,issue-var ,props-var ,adjust-var))
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
                         (string-trim-right
                          (format org-property-format
                                  (format ":%s:" (car prop))
                                  (or (cadr prop) ""))))
                       (seq-concatenate 'list issue-props creator-props extra-props)
                       "\n")
            "\n:END:")))

(defun jirassic-org--issue-properties (issue &optional extra-drawer-props description-level-adjust)
  "Return a plist of ISSUE props for template substitution.

EXTRA-DRAWER-PROPS is an alist of extra props to include in the
formatted org property drawer.

DESCRIPTION-LEVEL-ADJUST is the amount to promote the headings in the
serialized issue description by. Defaults to 1, which is appropriate
for `entry'-type capture templates that put the issue summary as a
level-1 heading and the description content under it. Pass 0 for
`plain'-type templates where the description sits at file level."
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
                                         issue-status
                                         nil #'equal)
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
                               (jira-issue-description issue)
                               (or description-level-adjust 1)))
          :issue-property-drawer issue-property-drawer)))

(defun jirassic-org--pull-ediff (source-buffer pull-buffer)
  "Ediff SOURCE-BUFFER against PULL-BUFFER, restoring window state on quit.

When the user quits ediff, SOURCE-BUFFER and PULL-BUFFER are killed, and
the window configuration captured at call time is restored."
  (let ((window-config (current-window-configuration)))
    (ediff-buffers source-buffer pull-buffer
                   (list (lambda ()
                           (add-hook 'ediff-cleanup-hook
                                     (lambda ()
                                       (dolist (buf (list source-buffer pull-buffer))
                                         (when (buffer-live-p buf)
                                           (kill-buffer buf)))
                                       ;; Restore the window config
                                       ;; after ediff's normal cleanup
                                       (run-with-timer 0 nil
                                                       (lambda () (set-window-configuration window-config))))
                                     nil t))))))

(defun jirassic-org--buffer-contents-equal (buf-a buf-b)
  "Return t if the contents of BUF-A and BUF-B are identical.

Trailing newlines are ignored when comparing."
  (cl-flet ((trimmed (buf)
              (with-current-buffer buf
                (string-trim-right
                 (buffer-substring-no-properties (point-min) (point-max))
                 "\n+"))))
    (string= (trimmed buf-a) (trimmed buf-b))))

;;;###autoload
(defun jirassic-org-pull ()
  "Pull the latest version of the Jira issue at point and ediff it locally.

This is meant to be used on a Jira issue that is captured as an org
entry. The issue is identified by the `issue-key' property on the
current entry. The remote issue is rendered with the org capture
template referenced by the `issue-template-key' property, falling back
to an interactive template prompt when that key no longer resolves. The
rendered result is then compared against the current subtree using
`ediff'.

Only `entry' and `plain' capture template types are supported, and the
template body must be a literal string."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (let* ((issue-key (or (org-entry-get nil "issue-key")
                        (user-error "No issue-key property on this entry")))
         (org-capture-templates jirassic-org-capture-templates)
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
         (template-type (nth 2 template-entry))
         (template-string (nth 4 template-entry)))
    (unless (memq template-type '(entry plain))
      (user-error
       "Unsupported capture template type `%s'; `jirassic-org-pull' only supports `entry' and `plain'"
       template-type))
    (unless (stringp template-string)
      (user-error
       "Unsupported capture template body; `jirassic-org-pull' requires a literal string template"))
    (when (and (eq template-type 'entry)
               (not (org-current-level)))
      (user-error "Point must be on or under a heading for `entry' templates"))
    (let* ((source-buffer (current-buffer))
           (source-entry-start (when (eq template-type 'entry)
                                 (save-excursion
                                   (org-back-to-heading t) (point))))
           (source-entry-level (when (eq template-type 'entry)
                                 (org-current-level)))
           (description-level-adjust (if (eq template-type 'entry) 1 0))
           (extra-drawer-props
            (when jirassic-org-store-template-key
              `(("issue-template-key" ,template-key))))
           (issue (aio-wait-for (jirassic-get-issue issue-key)))
           (pull-buffer (generate-new-buffer (format "*%s-latest*" issue-key)))
           (source-indirect (make-indirect-buffer source-buffer
                                                  (format "*%s-current*" issue-key)
                                                  t))
           ;; Track whether setup and handover to ediff was successful.
           (ediff-handover nil))
      (unwind-protect
          (progn
            ;; Expand the capture template into a temp buffer so we can
            ;; diff it with the source entry that we are trying to
            ;; update.
            (with-current-buffer pull-buffer
              (org-mode)
              (jirassic-org--with-capture-context issue
                  extra-drawer-props
                  description-level-adjust
                (let ((org-capture-plist (list :template template-string
                                               :buffer pull-buffer)))
                  (insert (org-capture-fill-template))
                  ;; Removes the `%?' cursor marker
                  (org-capture--position-cursor (point-min) (point-max))))
              (goto-char (point-min))
              ;; Make sure that both entries are at the same level
              (when (and (eq template-type 'entry)
                         (org-at-heading-p))
                (let ((delta (- source-entry-level (org-current-level))))
                  (cond ((> delta 0) (dotimes (_ delta) (org-demote-subtree)))
                        ((< delta 0) (dotimes (_ (- delta)) (org-promote-subtree))))))
              (set-buffer-modified-p nil))

            (when (eq template-type 'entry)
              (with-current-buffer source-indirect
                (goto-char source-entry-start)
                (org-narrow-to-subtree)))

            (if (jirassic-org--buffer-contents-equal source-indirect pull-buffer)
                (message "Issue %s has no new changes" issue-key)
              (jirassic-org--pull-ediff source-indirect pull-buffer)
              ;; At this point, the ediff session has started, and it will
              ;; clean up when the session ends.
              (setq ediff-handover t)))
        (unless ediff-handover
          (when (buffer-live-p pull-buffer) (kill-buffer pull-buffer))
          (when (buffer-live-p source-indirect) (kill-buffer source-indirect)))))))

(provide 'jirassic-org)
;;; jirassic-org.el ends here
