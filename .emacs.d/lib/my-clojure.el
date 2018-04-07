;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Clojure specific functions and configurations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'clojure-mode)
(require 'cider-mode)


(put 'describe 'clojure-backtracking-indent '(4 2))
(put 'it 'clojure-backtracking-indent '(4 2))
(put 'before 'clojure-backtracking-indent '(2))
(put 'before-all 'clojure-backtracking-indent '(2))
(put 'after-all 'clojure-backtracking-indent '(2))
(put 'after 'clojure-backtracking-indent '(2))

;;(add-hook 'after-save-hook 'cider-namespace-refresh)

(defun cider-namespace-refresh ()
  (interactive)
  (when (and (boundp 'cider-mode) cider-mode)
    (cider-interactive-eval
     "(require 'clojure.tools.namespace.repl)
  (clojure.tools.namespace.repl/refresh)")))

(defun my-clojure-mode-hook ()
  ;; Set Speclj indentaion
  (turn-on-eldoc-mode)
  (cider-mode 1))

(add-hook 'clojure-mode-hook 'my-clojure-mode-hook)


(define-key clojure-mode-map (kbd "C-c C-r") 'cider-namespace-refresh)


(define-key cider-mode-map (kbd"C-c C-c") nil)
(define-key cider-mode-map (kbd"C-c C-u") nil)
