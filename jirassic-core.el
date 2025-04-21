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


(provide 'jirassic-core)
;;; jirassic-core.el ends here
