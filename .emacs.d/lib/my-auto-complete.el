;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete-config)

(ac-config-default)

(global-auto-complete-mode t)

(setq ac-auto-start 0)         ;; 2
(setq ac-delay 0)              ;; 0.1
(setq ac-auto-show-menu 0)   ;; 0.8
(setq ac-quick-help-delay 0.1) ;; 1.5

;; bindings
(ac-set-trigger-key "TAB")

(define-key ac-complete-mode-map (kbd "C-m")   'ac-complete)
(define-key ac-complete-mode-map (kbd "C-i")   'ac-expand)

(define-key ac-complete-mode-map (kbd "C-c f") 'ac-fuzzy-complete)
(define-key ac-complete-mode-map (kbd "C-g")   'ac-stop)

(define-key ac-complete-mode-map (kbd "C-n")   'ac-next)
(define-key ac-complete-mode-map (kbd "C-p")   'ac-previous)

(setq ac-modes
      '(lisp-interaction-mode
        emacs-lisp-mode clojure-mode scheme-mode lisp-mode redshank-mode
        c-mode cc-mode c++-mode java-mode jde-mode perl-mode
        cperl-mode python-mode ruby-mode scala-mode
        ecmascript-mode javascript-mode js-mode js2-mode php-mode
        emms-tag-editor-mode asm-mode makefile-mode sh-mode
        fortran-mode f90-mode ada-mode xml-mode sgml-mode css-mode
        ocaml-mode tuareg-mode haskell-mode
        org-mode latex-mode text-mode eshell-mode
        erlang-mode erlang-shell-mode
        ))

(setq-default ac-sources
              '(ac-source-filename
                ;; ac-source-files-in-current-dir
                ac-source-words-in-buffer
                ;; ac-source-words-in-all-buffer
                ;; ac-source-words-in-same-mode-buffers
                ;;ac-source-yasnippet
                ;; ac-source-abbrev ac-source-dictionary
                ;; ac-source-gtags ac-source-imenu
                ;; ac-source-semantic ac-source-eclim
                ;; ac-source-features ac-source-functions
                ;; ac-source-symbols ac-source-variables
                ;; ac-source-rcodetools
                ;; ac-source-ispell
                ))

(defun ac-emacs-lisp-mode-setup ()
  (setq ac-sources (append
                    '(ac-source-dictionary
                      ac-source-features
                      ac-source-functions
                      ac-source-variables
                      ac-source-symbols) ac-sources))
  
  (setq ac-omni-completion-sources
        '(("\\<featurep\s+'" ac+-source-elisp-features)
          ("\\<require\s+'"  ac+-source-elisp-features)
          ("\\<load\s+\""    ac-source-emacs-lisp-features))))

(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)


(defun ac-erlang-mode-setup ()
  (setq ac-sources (append
                    '(
                      ;; ac-source-dictionary
                      ;; ac-source-words-in-same-mode-buffers
                      ) ac-sources)))

(add-hook 'erlang-mode-hook 'ac-erlang-mode-setup)


(defun ac-erlang-shell-mode-setup ()
  (setq ac-sources (append '(ac-source-dictionary
                             ac-source-words-in-same-mode-buffers
                             ) ac-sources)))

(add-hook 'erlang-shell-mode-hook 'ac-erlang-shell-mode-setup)


(defun ac-org-mode-setup ()
  (setq ac-sources (append '(ac-source-words-in-same-mode-buffers) ac-sources)))

(add-hook 'org-mode-hook 'ac-org-mode-setup)


;; (defun ac-haskell-mode-setup ()
;;   (require 'auto-complete-haskell)
;;   (require 'auto-complete-extension)
;;   (setq ac-sources (append '(my/ac-source-haskell ac-source-haskell) ac-sources)))

;; (add-hook 'haskell-mode-hook 'ac-haskell-mode-setup)


(defun ac-eshell-mode-setup ()
  (setq ac-sources (append '(ac-source-abbrev
                             ac-source-words-in-same-mode-buffers
                             ac-source-files-in-current-dir)
                           ac-sources)))

(add-hook 'eshell-mode-hook 'ac-eshell-mode-setup)


(defun ac-c-mode-setup ()
  (setq ac-sources (append '(ac-source-gtags ac-source-semantic) ac-sources))
  (setq ac-omni-completion-sources (cons "\\." '(ac-source-semantic)))
  (setq ac-omni-completion-sources (cons "\\->" '(ac-source-semantic))))

(add-hook 'c-mode-common-hook 'ac-c-mode-setup)


;; (defun ac-python-mode-setup ()
;;   (setq ac-sources (append '(ac-source-ropemacs) ac-sources))
;;   (add-to-list 'ac-omni-completion-sources (cons "\\." '(ac-source-ropemacs)))
;;   (add-to-list 'ac-omni-completion-sources (cons "\\=" '(ac-source-ropemacs))))

;; (add-hook 'python-mode-hook 'ac-python-mode-setup)


;; (defun ac-ruby-mode-setup ()
;;   (setq ac-sources (append '(ac-source-rcodetools) ac-sources))
;;   (add-to-list 'ac-omni-completion-sources (cons "\\." '(ac-source-rcodetools)))
;;   (add-to-list 'ac-omni-completion-sources (cons "\\=" '(ac-source-rcodetools))))

;; (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)

(provide 'my-auto-complete)