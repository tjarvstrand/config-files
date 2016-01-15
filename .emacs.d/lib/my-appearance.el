;; color-theme
(add-to-list 'load-path "~/elisp/themes")
(require 'color-theme)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window configuration
;; (defvar th-frame-config-register
;;   "The register which is used for storing and restoring frame
;; configurations by `th-save-frame-configuration' and
;; `th-jump-to-register'.")

;; (defun th-save-frame-configuration ()
;;   "Stores the current frame configuration in register
;; `th-frame-config-register'. If a prefix argument is given, you
;; can choose which register to use."
;;   (frame-configuration-to-register th-frame-config-register))

;; Set fullscreen
(when x-session-id
  (require 'fullscreen)
  (setq default-frame-alist
        '((top . 10) (left . 2)))
  (fullscreen))

(run-with-timer 0.2 nil #'(lambda () (split-windows-to-size 80)))

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
(setq-default truncate-lines t)
