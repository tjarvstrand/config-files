
;; Python mode
(add-to-list 'load-path "~/elisp/python-mode/")
(setq py-install-directory "~/elisp/python-mode/")
(require 'python-mode)

(setq py-split-windows-on-execute-p nil)
(setq py-shell-name "ipython")

(defun my-py-shell ()
  (interactive)
  (switch-to-buffer (py-shell nil nil nil nil nil nil t)))

(defun my-python-mode-hook ()
  (local-set-key (kbd "C-c C-c") 'comment-region)
  (local-set-key (kbd "C-c C-u") 'uncomment-region))
(add-hook 'python-mode-hook 'my-python-mode-hook)
