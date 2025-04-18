;;; setup.el --- Emacs configuration -*- lexical-binding: t -*-

(require 'bytecomp)
(require 'package)

;; Configure package
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("stable.melpa" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")))

(setq package-archive-priorities
      '(("stable.melpa" . 10)
        ("gnu"          . 10)
        ("nognu"        . 5)
        ("melpa"        . 10)))

(setenv "LSP_USE_PLISTS" "true")

(defvar bootstrap-packages
  '(delight
    use-package
    flycheck

    ;; dev
    find-file-in-project
    git-grep
    gnu-elpa-keyring-update
    magit
    projectile
    rg

    ;; completion
    corfu
    corfu-terminal

    ;; lsp related
    ;;company-lsp
    lsp-mode
    lsp-ui

    yasnippet

    ;; c/c++ related
    clang-format
    cmake-ide
    cmake-mode
    google-c-style

    ;; python related
    elpy
    pyimpsort
    python-black
    cython-mode

    ;; elisp
    buttercup

    ;; prog modes
    visual-fill-column
    go-mode
    graphviz-dot-mode
    lua-mode
    nginx-mode
    php-mode
    protobuf-mode
    yaml-mode

    ;; ;; vterm
    ;; vterm
    ;; multi-vterm

    ;; misc
    ace-jump-mode
    ace-window
    all-the-icons
    all-the-icons-completion
    gif-screencast
    i3wm-config-mode
    keycast
    ;; nano-modeline
    telephone-line
    pass
    ;;pdf-tools
    which-key
    zoom
    wgrep

    ;; vertico
    consult
    embark
    embark-consult
    marginalia
    vertico
    orderless

    ;; org-mode
    org-bullets

    ;; themes
    modus-themes
))

(defun byte-compile-site-lisp ()
  (let*
      ((user-emacs-directory (file-name-as-directory (elt argv 0)))
       (site-lisp-directory (concat user-emacs-directory "site-lisp"))
       (init-el-path (concat user-emacs-directory "init.el")))
    (message
     (format
      "Compiling emacs files, directory: %s" site-lisp-directory))
    (add-to-list 'load-path site-lisp-directory)
    (byte-recompile-directory site-lisp-directory 0)
    (message (format "Compiling emacs %s" init-el-path))
    (byte-recompile-file init-el-path 0)))

(defun install-packages ()
  (package-initialize)
  (message "Refreshing packages list...")
  (when (not (getenv "EMACS_PACKAGES_REFRESH_SKIP"))
    (package-refresh-contents))
  (dolist (package bootstrap-packages)
    (unless (package-installed-p package)
      (message (format "Installing package: %s" package))
      (package-install package t))))


(message "Running: %s" (emacs-version))

(install-packages)
(byte-compile-site-lisp)
;;; setup.el ends here
