(setq inhibit-x-resources t)

(let ((lib-dir (concat user-emacs-directory "/lib")))
  (add-to-list 'load-path lib-dir)
  (dolist  (dir (directory-files lib-dir t "^[^.]"))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ELPA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ))
(unless (file-directory-p "~/.emacs.d/elpa")
    (make-directory "~/.emacs.d/elpa"))
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

; list the packages you want
(setq package-list '(auto-highlight-symbol
                     color-theme
                     company
                     dash
                     ensime
                     erlang
                     f
                     popup
                     python-mode
                     s
                     sbt-mode
                     scala-mode
                     yaml-mode
                     yasnippet))

; install the missing packages
(dolist (package package-list)
  (package-install package t))


;;;;;;;;;;;;;;;;;;;;;
;; Misc


(load-library "my-misc")
(load-library "misc-cmds")
(load-library "show-point-mode")
;; (load-library "mismatched-parens")

(toggle-buffer-tail "*Messages*" "on")

(add-hook 'find-file-hook 'subword-mode)
(fset 'yes-or-no-p ' y-or-n-p)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-display))
 '(ahs-idle-interval 1)
 '(comment-multi-line t)
 '(create-lockfiles nil)
 '(custom-safe-themes
   (quote
    ("b04425cc726711a6c91e8ebc20cf5a3927160681941e06bc7900a5a5bfe1a77f" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(debug-on-error nil)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(edts-inhibit-package-check t)
 '(edts-man-root "~/.emacs.d/edts/doc/R15B03")
 '(electric-indent-mode nil)
 '(epushover-token "8voZIQ79jOdclr92TizvFcVr84gpnb")
 '(epushover-user-key "iExDziORD2VAffHJFa4nHh5BKlOYwZ")
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(message-log-max 10000)
 '(package-selected-packages
   (quote
    (auto-complete "s" "s" nil auto-highlight-symbol yaml-mode python-mode f erlang ensime color-theme)))
 '(safe-local-variable-values
   (quote
    ((py-smart-indentation)
     (python-indent . 2)
     (py-indent-offset . 2)
     (erlang-indent-level . 2)
     (allout-layout . t))))
 '(vc-follow-symlinks t))

(setq x-select-enable-clipboard 1)
(setq-default indent-tabs-mode nil)

(require 'smart-beginning-of-line)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
(load-library "my-keybindings")
(load-library "my-macros")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "my-appearance")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autosaves, backups etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "autosaves")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(menu-bar-mode 0)
(delete-selection-mode  1)
(transient-mark-mode    1)
(winner-mode 1)

;; Minibuffer
(setq max-mini-window-height 10)
(set-face-foreground 'minibuffer-prompt "white")
(set-face-bold-p 'minibuffer-prompt t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

(add-hook 'ido-minibuffer-setup-hook #'(lambda() (set (make-local-variable 'truncate-lines) nil)))
(add-hook 'ido-minibuffer-setup-hook #'(lambda() (enlarge-window 10)))

(setq ido-use-virtual-buffers t)
(setq ido-auto-merge-delay-time 0)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
;; (ido-everywhere t)
(ido-mode t)

(load-library "my-clojure")
(load-library "my-erlang")
(load-library "my-python")
(load-library "my-ruby")
(load-library "my-scala")

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(setq ac-auto-show-menu t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VC Git

(defadvice vc-git-working-revision (around vc-git-working-revision-detached activate)
  "Get the git working revision when detached"
  ad-do-it
  (when (string= ad-return-value "")
    (setq ad-return-value
          (with-output-to-string
            (with-current-buffer standard-output
              (vc-git--out-ok "describe" "--tags" "--exact-match" "HEAD")))))
  (when (string= ad-return-value "")
    (setq ad-return-value
          (with-output-to-string
            (with-current-buffer standard-output
              (vc-git--out-ok "rev-parse" "HEAD")))))
  (setq ad-return-value (replace-regexp-in-string "\n$" "" ad-return-value)))
(ad-activate 'vc-git-working-revision t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comint

(setq comint-buffer-maximum-size 15000)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kred

(setq edts-project-overrides
      '(("/home/tjarvstrand/klarna/kred" . (:otp-path "/home/tjarvstrand/.erlang.d/otp_17.5.6_kred"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
