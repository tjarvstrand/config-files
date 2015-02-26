
(defun kill-trailing-whitespace ()
  (interacive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\s+$" nil t)
      (replace-match "" nil nil))))

(defun split-windows-to-size (size)
  (while (> (window-width) (* 2 size))
    (split-window-horizontally (- (window-width) size)))
  (balance-windows))

(defun insert-filename (filename)
  (interactive "*FInsert filename: ")
  (insert filename))

(defun sudo-edit (&optional arg)
  "Edit file with superuser rights."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun sudo-edit-current-file ()
  "Edit the current file as root"
  (interactive)
  (let ((point (point)))
    (find-alternate-file
     (concat "/sudo:root@localhost:" (buffer-file-name (current-buffer))))
    (goto-char point)))

(defun symbol-next ()
  "Moves point to next occurrence, if any, of the symbol-at-point, if any"
  (interactive)
  (let ((mark          (point-marker))
        (symbol        (thing-at-point 'symbol))
        (symbol-bounds (bounds-of-thing-at-point 'symbol)))
    (unless symbol
      (error "no symbol at point"))
    (goto-char (1+ (cdr symbol-bounds)))
    (search-symbol-forward symbol)
    (when (and (>= (point) (car symbol-bounds))
               (<= (point) (cdr symbol-bounds)))
      (error "No more ocurrences of %s" symbol))
    (goto-char (match-beginning 0))
    (push-mark mark t)))

(defun search-symbol-forward (symbol)
  (let ((point (point)))
    (or (find-next-symbol-forward symbol (point-max))
        (progn
          (goto-char (point-min))
          (find-next-symbol-forward symbol point)))))

(defun find-next-symbol-forward (symbol bound)
  (let ((res nil)
        (case-fold-search nil))
    (while (and (< (point) bound) (not res))
      (when (and (search-forward symbol bound 'move-point) (not res)
                 (save-match-data (string= (thing-at-point 'symbol) symbol)))
        (setq res (match-end 0))))
    res))

(defun symbol-previous ()
  "Moves point to previous occurrence, if any, of the symbol-at-point, if any"
  (interactive)
  (let ((mark          (point-marker))
        (symbol        (thing-at-point 'symbol))
        (symbol-bounds (bounds-of-thing-at-point 'symbol)))
    (unless symbol
      (error "no symbol at point"))
    (goto-char (1- (car symbol-bounds)))
    (next-symbol-backward symbol)
    (when (and (>= (point) (car symbol-bounds))
               (<= (point) (cdr symbol-bounds)))
      (error "No more ocurrences of %s" symbol))
    (goto-char (match-beginning 0))
    (push-mark mark t)))

(defun next-symbol-backward (symbol)
  (let ((point (point)))
    (or (find-next-symbol-backward symbol (point-min))
        (progn
          (goto-char (point-max))
          (find-next-symbol-backward symbol point)))))

(defun find-next-symbol-backward (symbol bound)
  (let ((res nil)
        (case-fold-search nil))
    (while (and (> (point) bound) (not res))
      (when (and (search-backward symbol bound 'move-point) (not res)
                 (save-match-data (string= (thing-at-point 'symbol) symbol)))
        (setq res (match-end 0))))
    res))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tailing a buffer (even one not visiting a file)
;alist of 'buffer-name / timer' items
(defvar buffer-tail-alist nil)
(defun buffer-tail (name)
  "follow buffer tails"
  (cond ((or (equal (buffer-name (current-buffer)) name)
         (string-match "^ \\*Minibuf.*?\\*$" (buffer-name (current-buffer)))))
        ((get-buffer name)
      (with-current-buffer (get-buffer name)
        (goto-char (point-max))
        (let ((windows (get-buffer-window-list (current-buffer) nil t)))
          (while windows (set-window-point (car windows) (point-max))
         (with-selected-window (car windows) (recenter -3)) (setq windows (cdr windows))))))))

(defun toggle-buffer-tail (name &optional force)
  "toggle tailing of buffer NAME. when called non-interactively, a FORCE arg of 'on' or 'off' can be used to to ensure a given state for buffer NAME"
  (interactive (list (cond ((if name name) (read-from-minibuffer 
      (concat "buffer name to tail" 
        (if buffer-tail-alist (concat " (" (caar buffer-tail-alist) ")") "") ": ")
    (if buffer-tail-alist (caar buffer-tail-alist)) nil nil
           (mapcar #'(lambda (x) (car x)) buffer-tail-alist)
        (if buffer-tail-alist (caar buffer-tail-alist)))) nil)))
  (let ((toggle (cond (force force) ((assoc name buffer-tail-alist) "off") (t "on")) ))
    (if (not (or (equal toggle "on") (equal toggle "off"))) 
      (error "invalid 'force' arg. required 'on'/'off'") 
      (progn 
        (while (assoc name buffer-tail-alist) 
           (cancel-timer (cdr (assoc name buffer-tail-alist)))
           (setq buffer-tail-alist (remove* name buffer-tail-alist :key 'car :test 'equal)))
        (if (equal toggle "on")
            (add-to-list 'buffer-tail-alist (cons name (run-at-time t 1 'buffer-tail name))))
        (message "toggled 'tail buffer' for '%s' %s" name toggle)))))
