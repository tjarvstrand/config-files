
(defvar mismatched-parens-overlays nil)

(defun mismatched-parens-make-overlay (point)
  (let ((overlay (make-overlay point (1+ point))))
    (overlay-put overlay 'face 'show-paren-mismatch)
    (push overlay mismatched-parens-overlays)))

(defun mismatched-parens-clear-overlays ()
  (while mismatched-parens-overlays
    (delete-overlay (pop mismatched-parens-overlays))))

(defun mismatched-parens ()
  (mismatched-parens-clear-overlays)
  (save-restriction
    (widen)
    (let ((prev nil)
          (cur  (mismatched-parens-next (point-min))))
      (while (and cur (not (eq cur prev)))
        (mismatched-parens-make-overlay cur)
        (setq prev cur)
        (setq cur (mismatched-parens-next (1+ cur)))))))

(defun mismatched-parens-next (start-point)
  (condition-case data
      (prog1 nil
        (scan-sexps start-point (point-max)))
    (scan-error (nth 2 data))))
