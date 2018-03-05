;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Erlang specific functions and configurations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'load-path "~/elisp/erl-parse")
;; (require 'erl)
(add-to-list 'exec-path "/home/tjarvstrand/.erlang.d/current/bin")
(setq erlang-root-dir "/home/tjarvstrand/.erlang.d/curent/lib/erlang")
(require 'erlang-start)

;; Workaround for eqc-mode setting debug-on-error to t
(add-hook 'erlang-mode-hook (lambda () (setq debug-on-error nil)))

(setq erlang-root-dir "/home/tjarvstrand/.erlang.d/otp_17.5.6_kred/lib/erlang")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation
(add-to-list 'safe-local-variable-values '(erlang-indent-level . 2))

(setq edts-code-issue-wrap-around t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDTS
(add-to-list 'load-path "~/src/edts")
(add-to-list 'load-path "~/src/edts/elisp/edts")


;; (defun my-after-init-erlang-hook ()
;;   (require 'edts-start)
;;   (load-file "~/src/edts/test/load-tests.el"))
;; (add-hook 'after-init-hook 'my-after-init-erlang-hook)

;; (edts-log-set-level 'debug)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer setup
(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
(defun my-erlang-mode-hook ()
  (setq erlang-next-lines-empty-threshold 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set face of exported functions
(when (boundp 'erlang-font-lock-exported-function-name-face)
  (set-face-attribute 'erlang-font-lock-exported-function-name-face nil
                      :underline t))

(defun erl-print ()
  (interactive)
  (if (symbol-at-point)
      (erl-print-var)
    (erl-print-line)))

(defun erl-print-var ()
  (let ((var (symbol-at-point)))
    (erl-print-str
     (format (concat "io:fwrite(user, <<\"~p ~p ~p: %s = ~p~n\">>, "
                     "[self(), ?MODULE, ?LINE, %s])")
             var
             var))))

(defun erl-print-res ()
  (interactive)
  (erl-print-str
   "io:fwrite(user, <<\"~p ~p ~p: ~p~n\">>, [self(), ?MODULE, ?LINE, $])")
  (search-backward "$")
  (delete-char 1))

(defun erl-print-line ()
  (erl-print-str
   "io:fwrite(user, <<\"~p ~p ~p~n\">>, [self(), ?MODULE, ?LINE])"))

(defun erl-print-str (str)
  (end-of-line)
  (setq str (maybe-append-separator str))
  (unless (empty-line-p)
    (maybe-insert-preceeding-comma)
    (newline-and-indent))
  (insert str))

(defun maybe-insert-preceeding-comma ()
  (cond
   ((looking-back ",\\|->\\|\s-*of") (ignore))
   ((looking-back ";") (delete-char -1) (insert ","))
   (t                  (insert ","))))

(defun maybe-append-separator (str)
  (cond ((looking-back "\\(,\\|->\\)") (concat str ","))
        ((looking-back ";")      (concat str ";"))
        (t                       str)))
