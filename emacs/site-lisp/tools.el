;;; tools.el --- My own emacs tools -*- lexical-binding: t; -*-

(require 'mc-move)

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

(defun camelcase (&optional arg)
  (interactive "p")
  (dotimes (_ (or arg 1))
    (skip-chars-forward mc-move-delims)
    (skip-chars-forward mc-move-specials)
    (while
        (let ((start (point)))
          (when (> (+ (skip-chars-forward mc-move-upper-words)
                      (skip-chars-forward mc-move-lower-words))
                   0)
            (capitalize-region start (point)) t))
      (delete-backward-char (skip-chars-forward mc-move-specials)))))

(defun underscore (&optional arg)
  (interactive "p")
  (dotimes (_ (or arg 1))
    (skip-chars-forward mc-move-delims)
    (skip-chars-forward mc-move-specials)
    (while
        (let ((start (point)))
          (when (> (+ (skip-chars-forward mc-move-upper-words)
                      (skip-chars-forward mc-move-lower-words))
                   0)
            (downcase-region start (point)) t))
      (delete-backward-char (skip-chars-forward mc-move-specials))
      (when (looking-at-p "[[:alnum:]]")
        (insert "_")))))

(provide 'tools)
;;; tools.el ends here
