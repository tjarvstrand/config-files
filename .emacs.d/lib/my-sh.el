
(defun my-sh-mode-hook ()
  (local-set-key (kbd "C-c C-c") 'comment-region)
  (local-set-key (kbd "C-c C-u") 'uncomment-region))
(add-hook 'sh-mode-hook 'my-sh-mode-hook)
