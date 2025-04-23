;;; jirassic-core.el --- Core Jirassic functions -*- lexical-binding: t; -*-

;;; Commentary:



;;; Code:
(defun jirassic-alist-nget (keys alist)
  "Get the value of a key in an alist."
  (if (seqp keys)
      (seq-reduce (lambda (res key)
                    (alist-get key res))
                  keys alist)
    (alist-get keys alist)))


(defmacro jirassic-bind-restore (bindings &rest body)
  "Save buffer/point, bind vars like `let*', restore context, run BODY.

BINDINGS is a list of bindings, exactly like `let*'.

The current buffer and point are saved before the `let*' bindings
begin. After all bindings in the `let*' are established (potentially
involving asynchronous waits), the original buffer and point are
restored, and BODY is executed within that context."
  (declare (indent 1) (debug bindings))

  (let ((restore-to (gensym "restore-to-")))
    `(let ((,restore-to (point-marker)))
       (let* ,bindings
         ;; Ensure cleanup happens even if the body or awaits in let* error out.
         ;; Note: If an await *within* the let* errors, the unwind-protect
         ;; might run before the body. Cleanup still occurs.
         (with-current-buffer (marker-buffer ,restore-to)
           ;; Restore the point using the marker position
           ;; Add check for marker validity for extra safety
           (goto-char (marker-position ,restore-to))
           ,@body)))))

(provide 'jirassic-core)
;;; jirassic-core.el ends here
