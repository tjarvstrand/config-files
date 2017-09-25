;; Twilight Colour Theme for Emacs.
;;
;; Defines a colour scheme resembling that of the original TextMate Twilight colour theme.
;; To use add the following to your .emacs file (requires the color-theme package):
;;
;; (require 'color-theme)
;; (color-theme-initialize)
;; (load-file "~/.emacs.d/twilight-emacs/color-theme-twilight.el")
;;
;; And then (color-theme-twilight) to activate it.
;;
;; Several areas still require improvement such as recognition of code that ruby-mode doesn't
;; yet pick up (eg. parent classes), Rails/Merb keywords, or non Ruby code related areas
;; (eg. dired, HTML, etc). Please feel free to customize further and send in any improvements,
;; patches most welcome.
;;
;; MIT License Copyright (c) 2008 Marcus Crafter <crafterm@redartisan.com>
;; Credits due to the excellent TextMate Twilight theme
;;
;; Thanks to Travis Jeffery for ido-mode and fixes to the minibuffer-prompt to fit in with the rest of the theme
;;

(require 'color-theme)

(defun color-theme-my-theme ()
  "Color theme by Marcus Crafter, based off the TextMate Twilight theme, created 2008-04-18"
  (interactive)
  (color-theme-install
	'(color-theme-my-theme
	  ((background-color . "#1C1C1C")
           (background-mode . dark)
           (border-color . "black")
           (cursor-color . "#E5D759")
           (foreground-color . "#E6E1DC")
           (mouse-color . "white"))

	  (blue ((t (:foreground "blue"))))
	  (border-glyph ((t (nil))))
	  (buffers-tab ((t (:background "#141414" :foreground "#CACACA"))))
	  (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
	  (font-lock-regexp-grouping-construct ((t (:foreground "red"))))
	  (gui-element ((t (:background "#D4D0C8" :foreground "black"))))
	  (highline-face ((t (:background "SeaGreen"))))
	  (ido-first-match ((t (:foreground "#8F9D6A"))))
	  (ido-only-match ((t (:foreground "#8F9D6A"))))
	  (ido-subdir ((t (:foreground "#CF6A4C"))))
	  (left-margin ((t (nil))))
	  (minibuffer-prompt ((t (:foreground "white") (:weight 'bold))))
	  ;; (mode-line ((t (:background "grey75" :foreground "black"))))
	  (mumamo-background-chunk-submode ((t (:background "#222222"))))
	  (text-cursor ((t (:background "yellow" :foreground "black"))))
	  (toolbar ((t (nil))))
	  (zmacs-region ((t (:background "snow" :foreground "blue"))))
          (bold ((t (:bold t))))
          (bold-italic ((t (:italic t :bold t))))
          (default ((t (nil))))
          (font-lock-builtin-face ((t (:foreground "#E1C582"))))
          (font-lock-comment-face ((t (:foreground "#AC4BB8" :italic t))))
          (font-lock-constant-face ((t (:foreground "#68C1D8"))))
          (font-lock-doc-string-face ((t (:foreground "red"))))
          (font-lock-function-name-face ((t (:foreground "#FFFFFF"))))
          (font-lock-keyword-face ((t (:foreground "#FC803A"))))
          (font-lock-preprocessor-face ((t (:foreground "#FC803A"))))
          (font-lock-reference-face ((t (:foreground "red"))))
          (font-lock-string-face ((t (:foreground "#8EC65F"))))
          (font-lock-type-face ((t (:foreground "white"))))
          (font-lock-variable-name-face ((t (:foreground "#E1C582"))))
          (font-lock-warning-face ((t (:foreground "red"))))
          (highlight ((t (:background "#282828"))))
          (italic ((t (:italic t))))
          (modeline ((t (:background "#8B8B8B" :foreground "#FFFFFF" ))))
          (modeline-inactive ((t (:background "#1C1C1C" :foreground "#555555"))))
          (region ((t (:background "#939393"))))
          (textile-link-face ((t (:foreground "#8EC65F"))))
          (textile-ol-bullet-face ((t (:foreground "#FC803A"))))
          (textile-ul-bullet-face ((t (:foreground "#FC803A"))))
          (underline ((t (:underline t))))
)))
