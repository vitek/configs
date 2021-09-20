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

(defconst progcase-separators "[_\\-]")

(defmacro progcase--make-conversion (arg conversion &optional tail)
  `(dotimes (_ (or ,arg 1))
     (skip-chars-forward mc-move-delims)
     (skip-chars-forward mc-move-specials)
     (let (start)
       (while
           (progn
             (setq start (point))
             (> (+ (skip-chars-forward mc-move-upper-words)
                   (skip-chars-forward mc-move-lower-words))
                0))
         ,conversion
         (delete-backward-char (skip-chars-forward progcase-separators))
         ,(when tail tail)))))

(defun camelcase (&optional arg)
  (interactive "p")
  (progcase--make-conversion arg
                             (capitalize-region start (point))))

(defun underscore-lowercase (&optional arg)
  (interactive "p")
  (progcase--make-conversion arg
                             (downcase-region start (point))
                             (when (looking-at-p "[[:alnum:]]") (insert "_"))))

(defun underscore-uppercase (&optional arg)
  (interactive "p")
  (progcase--make-conversion arg
                             (upcase-region start (point))
                             (when (looking-at-p "[[:alnum:]]") (insert "_"))))

(defalias 'underscore 'underscore-lowercase)

(provide 'tools)
;;; tools.el ends here
