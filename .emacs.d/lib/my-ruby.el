;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ruby specific functions and configurations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom ruby-mode-regexps
  '("Vagrantfile")
  "Additional extensions for which to auto-activate ruby-mode.")

(mapcar
 #'(lambda(re) (add-to-list 'auto-mode-alist (cons re 'ruby-mode)))
 ruby-mode-regexps)

(setq ruby-insert-encoding-magic-comment nil)
(setq ruby-indent-level 2)

(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\.lock" . ruby-mode))
