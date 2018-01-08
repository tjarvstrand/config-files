
(require 'ensime)

(define-key ensime-mode-map (kbd "M-p") nil)
(define-key ensime-mode-map (kbd "M-n") nil)

(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-mode-map (kbd "C-TAB") 'company-select-next)
