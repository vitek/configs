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

(provide 'my)
;;; my.el ends here
