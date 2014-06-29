(require 'trace)
(require 'edebug)

(defun trace-current-buffer (buffer)
  (interactive (list (current-buffer)))
  (setq edebug-trace t)
  (if (eq major-mode 'emacs-lisp-mode)
      (let ((edebug-all-defs t)) (eval-buffer))
    (message "%s is not an Elisp buffer." (current-buffer))))

(defun trace-current-buffer-stop (buffer)
  (interactive (list (current-buffer)))
  (setq edebug-trace nil)
  (if (eq major-mode 'emacs-lisp-mode)
      (let ((edebug-all-defs nil)) (eval-buffer))
    (message "%s is not an Elisp buffer." (current-buffer))))

(defun trace-all-buffers ()
  (interactive)
  (mapc
   #'(lambda (buf) (when (buffer-live-p buf) (trace-current-buffer buf)))
   (buffer-list)))

(defun trace-all-buffers-stop
  (interactive)
  (mapc
   #'(lambda (buf) (when (buffer-live-p buf) (trace-current-buffer-stop buf)))
   (buffer-list)))

(provide 'my-trace)
