(require 'bytecomp)
(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("stable.melpa" . "http://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))
(defvar bootstrap-packages
  '(clang-format
    cmake-ide
    cmake-mode
    color-theme-modern
    company-lsp
    find-file-in-project
    go-mode
    delight
    flycheck
    gnu-elpa-keyring-update
    helm
    lsp-mode
    ;;lsp-treemacs
    ;;lsp-ui
    lua-mode
    projectile
    pyimpsort
    python-black
    use-package
    yaml-mode))

(defun byte-compile-site-lisp ()
  (let*
      ((user-emacs-directory (file-name-as-directory (elt argv 0)))
       (site-lisp-directory (concat user-emacs-directory "site-lisp"))
       (init-el-path (concat user-emacs-directory "init.el")))
    (message
     (format
      "Compiling emacs files, directory: %s" site-lisp-directory))
    (add-to-list 'load-path site-lisp-directory)
    (message (format "Compiling emacs %s" init-el-path))
    (byte-recompile-file init-el-path 0)))

(defun install-packages ()
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
  ;; See http://elpa.gnu.org/packages/gnu-elpa-keyring-update.html
  ;;(setq package-check-signature nil)
  (package-initialize)
  (unless package-archive-contents
    (message "Refreshing packages list...")
    (package-refresh-contents))
  (dolist (package bootstrap-packages)
    (unless (package-installed-p package)
      (message (format "Installing package: %s" package))
      (package-install package t))))

(install-packages)
(byte-compile-site-lisp)
