(defcustom git-grep-command "git --no-pager grep -In -i "
  "Git grep command."
  :type 'string)

(when (and (require 'grep nil t)
           (require 'vc-git nil t))
  (defun git-grep (command-args)
    (interactive
     (progn
       (grep-compute-defaults)
       (if git-grep-command
           (list (read-shell-command "Run git-grep (like this): "
                                     git-grep-command 'git-grep-history))
         ;; No default was set
         (read-string
          "compile.el: No `git-grep-command' command available. Press RET.")
         (list nil))))
    (when command-args
      (let ((null-device nil)) ; see grep
        (grep command-args)))))

(provide 'git-grep)
