;; color-theme
(load-library "my-theme.el")
(color-theme-my-theme)

;; Set font
(defun set-font-size (size)
  (interactive "nSize: ")
  (set-frame-font (format "Ubuntu Mono-%s" size)))

(if (string= "brunsnultra" (system-name))
    (set-font-size 10)
  (set-font-size 9))

(global-font-lock-mode t)

;; (run-with-timer 0.2 nil #'(lambda () (split-windows-to-size 80)))

;; Line numbers on
(global-linum-mode 1)
(line-number-mode 1)
(column-number-mode 1)

;; Hide toolbar
(tool-bar-mode -1)

;; Remove most of the fringe
(set-frame-parameter nil 'right-fringe 1)

;; Spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; highlight matching parantheses
(show-paren-mode t)

;; Column marker to show when text crosses column 80
(require 'column-marker)
(add-hook 'find-file-hook (lambda () (interactive) (column-marker-3 80)))

;; Highlight current line
(global-hl-line-mode 1)

(setq-default whitespace-mode 1)
(setq whitespace-style       (quote (face tabs tab-mark lines-tail)))
(setq whitespace-display-mappings '((tab-mark 9 [9655 9] [92 9])))

(scroll-bar-mode -1)
