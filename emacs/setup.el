(setq site-lisp-directory (elt argv 0))
(message (format "Byte-compiling emacs files, directory: %s" site-lisp-directory))

(add-to-list 'load-path site-lisp-directory)
(byte-recompile-directory site-lisp-directory 0)
