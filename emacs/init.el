;;; init.el --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; https://github.com/purcell/emacs.d/blob/master/init.el
;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(require 'cl-lib)
(require 'package)
(require 'seq)

;; Interface decorations
(setq
 frame-title-format "emacs: %b"
 cursor-type 'bar
 inhibit-startup-screen t
 menu-bar-mode nil
 scroll-bar-mode nil
 tool-bar-mode nil
 use-dialog-box nil
 use-file-dialog nil
 visible-bell t

 jit-lock-defer-time 0.05

 ;; scroll speed
 scroll-conservatively 50
 scroll-up-agressively 0
 scroll-down-agressively 0)

;; Configure window info line
(setq
 display-time-24hr-format t
 display-time-default-load-average nil
 column-number-mode t)

;; Handle x11 clipboard
(setq x-select-enable-clipboard t)

;; Backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )
(setq create-lockfiles nil)

(save-place-mode 1)

;; TODO: cleanup this mess
(setq
 load-home-init-file t
 case-fold-search t
 compile-command "make "
 current-language-environment "UTF-8"
 font-lock-maximum-decoration t
 log-edit-hook (quote ()))

(custom-set-faces
 '(diff-added ((t (:foreground "Green"))) 'now)
 '(diff-removed ((t (:foreground "Red"))) 'now)

 '(show-ws-trailing-whitespace ((t (:background "Red"))) 'now)
 '(show-ws-tab ((t (:background "#222"))) 'now))

(defun set-executable (key choices)
  (let ((value (seq-find 'executable-find choices)))
    (when value
      (message (format "Executable %s chosen from %s" value choices))
      (set key value))))

;; Load custom file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t)

;; Configure package
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("stable.melpa" . "http://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))
(setq package-enable-at-startup nil) ; Don't initialize later as well
(package-initialize)

;; Require some packages
(use-package git-grep
  :bind (([f12] . git-grep)))

(use-package yaml-mode
  :config
  (setq yaml-indent-offset 4))

(use-package js
  :config
  (setq js-indent-level 2))

(use-package find-file-in-project)
(use-package mc-move)

(setq c-basic-offset 4)
(c-set-offset 'arglist-intro '+)
(c-set-offset 'inextern-lang 0)
;;(setq truncate-lines nil)
(setq kill-whole-line t)
(setq default-input-method "russian-computer")
(setq grep-command "grep -Eni ")
(setq compilation-environment '("LC_ALL=C"))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq-default sgml-basic-offset 2)
(setq-default py-indent-offset 4)
(setq py-smart-indentation nil)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(global-font-lock-mode t)
(show-paren-mode t)
(global-auto-revert-mode)
(blink-cursor-mode 0)
(line-number-mode t)
(display-time)
;;(display-battery)
;;(transient-mark-mode)
;;(if window-system (global-hl-line-mode))

;; Color theme setup, 0.1s
(use-package color-theme-modern
  :config
  (if window-system
      (progn
        (load-theme 'dark-laptop))))

(defun my-compile()
  (interactive)
  (if (fboundp 'projectile-compile-project)
      (call-interactively 'projectile-compile-project)
    (call-interactively 'compile)))

(defun nop() (interactive))

(defun default-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq c-basic-offset 4)
  (setq truncate-lines nil))

(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq c-basic-offset 8)
  (setq truncate-lines nil))

(defun delete-frame-or-kill-emacs (&optional arg)
  "Delete frame or kill Emacs if current frame is the last one."
  (interactive "P")
  (if (> (length (seq-filter 'frame-visible-p (frame-list))) 1)
      (delete-frame)
    (progn
      ;; Workarond against lsp-mode server restart prompt
      ;; see https://github.com/emacs-lsp/lsp-mode/issues/641
      (setq lsp-restart 'ignore)

      (if (y-or-n-p (format "Really want to quit emacs? "))
          (save-buffers-kill-emacs)
        (message "Not quiting emacs"))
      ;;(save-some-buffers arg t)
      ;;(kill-emacs)
      )))

(defun derived-mode-parents (mode)
  (and mode
       (cons mode (derived-mode-parents
                   (get mode 'derived-mode-parent)))))

(defun highlight-prog ()
  (cl-intersection (derived-mode-parents major-mode)
                '(prog-mode text-mode cmake-mode)))

;; Highlight whitespaces and long strings
(defun highlight-whitespaces ()
  (when (highlight-prog)
    (when (fboundp 'show-ws-highlight-trailing-whitespace)
      (show-ws-highlight-trailing-whitespace)
      (show-ws-highlight-tabs))
    ;; Show marker at 80 column
    (when (fboundp 'column-marker-1)
      (column-marker-1 79))))

(add-hook 'font-lock-mode-hook 'highlight-whitespaces)

(use-package show-wspace)
(use-package column-marker)

;; Unique buffer names
(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'post-forward))

;; Python-mode settings
(defvar py-flake8-history nil) ; workaround python-mode.el bug
(setq py-flake8-command-args
      '("--ignore=E12,F403" "--max-line-length=120" "--max-complexity=73"))

(set-executable 'python3-executable
                '("taxi-python3" "python3.7" "python3"))

(defun ipdb-insert-set-trace()
  (interactive)
  (insert "from pdb import set_trace; set_trace()"))

(use-package python
  :bind (:map python-mode-map
              ("C-c b" . ipdb-insert-set-trace)))

(use-package python-black
  :init
  (set-executable 'python-black-command
                  '("taxi-black" "black"))
  :after python
  :bind (:map python-mode-map ("C-c f" . python-black-buffer)))

(use-package pyimpsort
  :init
  ;; TODO: use own patched pyimpsort.py
  (setq pyimpsort-script
        (concat user-emacs-directory "scripts/pyimpsort.py")))

;; C/C++ mode settings
(add-hook 'c++-mode-hook
          (lambda ()
            (setq-local flycheck-gcc-language-standard "c++17")
            (setq-local flycheck-clang-language-standard "c++17")))

(use-package google-c-style
  :hook (c++-mode . (lambda ()
                      (google-set-c-style)
                      (c-set-offset 'inlambda 0))))

(use-package clang-format
  :init
  (set-executable 'clang-format-executable
                  '("clang-format-7" "clang-format"))
  :after cc-mode
  :bind (:map c++-mode-map ("C-c f" . clang-format-buffer)))

;; Lua
(use-package manual-indent
  :hook
  (lua-mode-hook . (lambda ()
                     (setq lua-indent-level 4)
                     (manual-indent-mode 1)
                     (electric-indent-local-mode 0))))

;; Enable flycheck globally
(use-package flycheck
  :init
  (when python3-executable
    (setq flycheck-python-flake8-executable python3-executable
          flycheck-python-pycompile-executable python3-executable
          flycheck-python-pylint-executable python3-executable))
  :config
  (global-flycheck-mode)
  :delight (flycheck-mode "/flycheck" "flycheck"))

;; lsp-mode setup
(use-package lsp-mode
  :init
  ;; clangd
  (set-executable 'lsp-clients-clangd-executable
                  '(;;"clangd-10"
                    "clangd-9" "clangd"))
  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))

  ;; pyls
  (set-executable 'vitja-lsp-pyls-server-command
                  '("/usr/lib/yandex/taxi-py3-2/bin/pyls" "pyls"))
  (setq lsp-pyls-server-command vitja-lsp-pyls-server-command)
  (setq xref-prompt-for-identifier nil)
  (setq lsp-enable-links nil)

  (add-hook
   'hack-local-variables-hook
   (lambda ()
     (when (boundp 'vitja-lsp-python-path)
       (setq-local
        lsp-pyls-server-command
        (list "/bin/sh" "-c"
              (concat
               "PYTHONPATH='"
               (string-join
                ;; filter out bad parts
                (seq-filter
                 (lambda (string)
                   (if (string-match "['\"\\$:;]" string)
                       (progn
                         (message "Dangerous PYTHONPATH part %s" string) nil)
                     t))
                 vitja-lsp-python-path)
                ":") "' " vitja-lsp-pyls-server-command))))))

  ;; golang
  (set-executable 'lsp-gopls-server-path '("gopls" "/home/vitja/go/bin/gopls"))

  ;; common settings
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-snippet nil)

  :commands lsp
  :hook ((python-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (go-mode . lsp-deferred)))

;; delight, tune minor mode bar
(use-package delight
  :config
  (delight '((abbrev-mode nil "abbrev")
             (company-mode nil "company")
             (eldoc-mode nil "eldoc")
             (yas-minor-mode nil "yasnippet")
             (lsp-mode "/lsp" "lsp")
             (python-mode "py" "python-mode")
             ;; File mode specification error: (wrong-type-argument stringp (inhibit-mode-name-delight C++//l c++))
             ;;(c++-mode "c++" "cc-mode")
             ))
  )

(use-package go-mode
  :hook
  (go-mode
   . (lambda ()
       (setq tab-width 4)
       (setq indent-tabs-mode 1)))
  :bind (:map go-mode-map
              ("C-c f" . gofmt)))

;; Dired setup
(add-hook 'dired-mode-hook
          (lambda ()
            (load "dired-x")
            (hl-line-mode 1)
            (dired-omit-mode 1)))
(setq dired-listing-switches "-alhv --group-directories-first")
(setq dired-isearch-filenames 'dwim)


;; ido-mode
(setq ido-enable-flex-matching t)
(setq ido-default-buffer-method 'selected-window)
(ido-mode 1)

;; Custom keybindings
(global-set-key (kbd "C-c #") 'comment-region)
(define-key ctl-x-map "\C-c" 'delete-frame-or-kill-emacs)

(global-set-key [C-prior] 'previous-buffer)
(global-set-key [C-next] 'next-buffer)
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

(global-set-key [f9] 'my-compile)
(global-set-key [f2] 'save-buffer)
(global-set-key [f3] 'find-file)
(global-set-key [f4] 'gud-gdb)
(global-set-key [f7] 'gud-step)
(global-set-key [f8] 'gud-next)
(global-set-key [C-f8] 'gud-break)
(global-set-key [f5] 'goto-line)

(if window-system
    (progn
      (global-set-key "\C-z" 'ignore)
      (global-set-key [mouse-4] 'scroll-down)
      (global-set-key [mouse-5] 'scroll-up)
      (global-set-key [mouse-2] 'nop)
      (global-set-key [C-f9] 'recompile)))

;; https://github.com/emacsmirror/zoom-frm
(use-package zoom-frm
  :bind
  ("C-M-=" . zoom-in)
  ("C-M--" . zoom-out)
  ("C-M-0" . zoom-frm-unzoom)
  ;; mouse bindings
  ("<C-mouse-4>" . zoom-in)
  ("<C-mouse-5>" . zoom-out))

(use-package projectile
  :init
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; Emacs server
(if window-system (server-start))

;; Load machine local configuration (if available)
(load (concat user-emacs-directory "local.el") t)

(message (format "gcs-done: %d" gcs-done))
(message (format "Emacs init time: %s" (emacs-init-time)))

(provide 'init)
;;; init.el ends here
