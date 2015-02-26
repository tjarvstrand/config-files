;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Clajure specific functions and configurations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(put 'describe 'clojure-backtracking-indent '(4 2))
(put 'it 'clojure-backtracking-indent '(4 2))
(put 'before 'clojure-backtracking-indent '(2))
(put 'before-all 'clojure-backtracking-indent '(2))
(put 'after-all 'clojure-backtracking-indent '(2))
(put 'after 'clojure-backtracking-indent '(2))

(defun my-clojure-mode-hook ()
  ;; Set Speclj indentaion
  (turn-on-eldoc-mode)
  (cider-mode 1))

(add-hook 'clojure-mode-hook 'my-clojure-mode-hook)
