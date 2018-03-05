(require 'ensime)

(require 'projectile)

(add-to-list 'load-path "/home/tjarvstrand/src/emacs-neotree")

;; (require 'find-file-in-project)
;; (make-variable-buffer-local 'ffip-patterns)
;; (make-variable-buffer-local 'ffip-prune-patterns)
;; (setq ffip-prefer-ido-mode t)

(require 'auto-highlight-symbol)
(add-to-list 'ahs-exclude (cons 'scala-mode scala-syntax:keywords-unsafe-re))

(defun my-ensime-mode-hook ()
  (projectile-mode)
  (auto-highlight-symbol-mode 1)
  ;; (setq ffip-patterns '("*.scala"
  ;;                       "*.java"
  ;;                       "*.sbt"
  ;;                       "*.json"
  ;;                       "*.xml"))

  ;; (setq ffip-prune-patterns (concat ffip-prune-patterns
  ;;                                   '("target")))
  )
(add-hook 'ensime-mode-hook 'my-ensime-mode-hook)
(defun my-scala-mode-hook ()
  (auto-highlight-symbol-mode 1)
  ;; (unless (ensime-connected-p)
  ;;   (ensime))
  )
(add-hook 'scala-mode-hook 'my-scala-mode-hook)

(define-key ensime-mode-map (kbd "M-p") nil)
(define-key ensime-mode-map (kbd "M-n") nil)

(define-key ensime-mode-map (kbd "C-c C-n") 'ensime-forward-note)
(define-key ensime-mode-map (kbd "C-c C-p") 'ensime-backward-note)

(define-key company-active-map (kbd "C-S-n") 'company-select-next)
(define-key company-active-map (kbd "C-S-p") 'company-select-previous)
(define-key company-mode-map (kbd "C-S-n") 'company-select-next)
