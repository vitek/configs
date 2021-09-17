(defun insert--timestamp (format)
  (insert (format-time-string format (current-time))))

(defun insert-timestamp ()
  (interactive)
  (insert--timestamp "%s"))

(defun insert-datetime ()
  (interactive)
  (insert--timestamp "%F %T%:z"))

(defun insert-date ()
  (interactive)
  (insert--timestamp "%F"))

(defun insert-python-datetime ()
  (interactive)
  (insert--timestamp "datetime.datetime(%-Y, %-m, %-d, %-H, %-M, %-S)"))

(defun insert-rfc-datetime ()
  (interactive)
  (insert
   (with-temp-buffer
     (shell-command "date -R" (current-buffer))
     (move-end-of-line 1)
     (buffer-substring-no-properties 1 (point)))))
