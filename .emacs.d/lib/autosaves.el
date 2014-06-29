;; save minibuffer history
(setq savehist-file "~/.emacs.d/savehist")
(setq savehist-additional-variables '(kill-ring regexp-search-ring search-ring))
(savehist-mode t)

;; backups
(setq backups-dir "~/.emacs.d/backups")
(setq backup-directory-alist
      `((".*" . ,backups-dir)))

(setq make-backup-files t)
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)

;; auto-saves
(setq auto-save-dir "~/.emacs.d/autosaves")
(setq auto-save-file-name-transforms
      `((".*" ,auto-save-dir t)))
(setq auto-save-default t)
