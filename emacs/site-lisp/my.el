;;; my.el --- My own emacs tools -*- lexical-binding: t; -*-

(defun my-compile()
  (interactive)
  (if (fboundp 'projectile-compile-project)
      (call-interactively 'projectile-compile-project)
    (call-interactively 'compile)))

(defun my-grep()
  (interactive)
  (let ((file (or (buffer-file-name) default-directory)))
    (cond
     ((and (fboundp 'counsel-git-grep) file (vc-find-root  file ".git"))
      (call-interactively 'counsel-git-grep))
     ((fboundp 'counsel-rg)
      (call-interactively 'counsel-rg))
     (t (call-interactively 'grep)))))

;; https://stackoverflow.com/questions/5194294/how-to-remove-all-newlines-from-selected-region-in-emacs
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))


(defun unfill-paragraph-copy (&optional region)
  (interactive)
  (message region)
  )


;; handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

(provide 'my)
;;; my.el ends here
