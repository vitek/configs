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

;; (setq use-package-verbose 'debug)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(require 'cl-lib)
(require 'package)
(require 'seq)

;; Interface decorations
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Cursor
(blink-cursor-mode 0)
(setq cursor-type 'bar)

;; Text-mode settings
(set-display-table-slot standard-display-table 'vertical-border ?│)

;; Modeline settings
(line-number-mode t)
(column-number-mode t)
(display-time)
;;(display-battery)

(setq
 frame-title-format "emacs: %b"
 inhibit-startup-screen t

 use-dialog-box nil
 use-file-dialog nil

 visible-bell t

 jit-lock-defer-time 0.05

 ;; Localization settings
 default-input-method "russian-computer"
 current-language-environment "UTF-8"

 ;; Search
 case-fold-search t

 ;; Edit
 kill-whole-line t

 ;; Scroll speed
 scroll-conservatively 10
 scroll-up-aggressively 0
 scroll-down-aggressively 0

 ;; Configure window info line
 display-time-24hr-format t
 display-time-default-load-average nil

 ;; Calendar
 calendar-week-start-day 1

 ;; Handle x11 clipboard
 select-enable-clipboard t)

;; Basic emacs modes modes
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)
(show-paren-mode t)
(global-auto-revert-mode t)
(save-place-mode t)

(setq c-basic-offset 4)
(c-set-offset 'arglist-intro '+)
(c-set-offset 'inextern-lang 0)
;;(setq truncate-lines nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq-default sgml-basic-offset 2)
(setq-default py-indent-offset 4)
(setq py-smart-indentation nil)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )
(setq create-lockfiles nil)

(setq
 log-edit-hook (quote ()))

(custom-set-faces
 '(whitespace-trailing ((t (:background "Red"))) 'now)
 '(whitespace-tab ((t (:background "#433" :inverse-video nil))) 'now)
 '(whitespace-line ((t (:background "gray"))) 'now))

(defun set-executable (key choices)
  (let ((value (seq-find 'executable-find choices)))
    (when value
      (message (format "Executable %s chosen from %s" value choices))
      (set key value))))

;; Load custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; Configure package
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("stable.melpa" . "http://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/"))
                                        ; Don't initialize later as well
      package-enable-at-startup nil)

(when (< emacs-major-version 27)
  (package-initialize))

;; Color theme
(load-theme 'zenburn t)
;; (use-package color-theme-modern
;;   :init
;;   ;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;;   (load-theme 'zenburn))
;;   ;; (if window-system
;;   ;;     (progn
;;   ;;       ;;(load-theme 'dark-laptop)
;;   ;;       ;;(load-theme 'classic)
;;   ;;       (setq solarized-scale-org-headlines nil
;;   ;;             solarized-scale-outline-headlines nil
;;   ;;             solarized-use-variable-pitch nil
;;   ;;             solarized-use-less-bold t)
;;   ;;       (load-theme 'solarized-dark))))

(use-package git-grep)
(use-package rg
  :config
  (rg-define-search rg-word
  :format literal
  :flags ("--word-regexp")
  :menu ("Custom" "w" "Word")))

(use-package yaml-mode
  :defer t
  :config
  (setq yaml-indent-offset 4))

(use-package js
  :defer t
  :config
  (setq js-indent-level 2))

(use-package find-file-in-project :defer t)
(use-package mc-move
  :defer t
  :config
  (global-mc-move-mode)
  :bind
  (("M-f"   . mc-move-forward-word)
   ("M-b"   . mc-move-backward-word)
   ("M-d"   . mc-move-kill-word)
   ("M-DEL" . mc-move-backward-kill-word)
   ("M-@"   . mc-move-mark-word)))

(use-package tools
  :defer t
  :bind
  (("C-c f"   . window-config-toggle)

   ("C-c M-u" . underscore-uppercase)
   ("C-c M-l" . underscore-lowercase)
   ("C-c M-c" . camelcase)))

(use-package ansi-color
  :config
  (defun my/ansi-colorize-buffer ()
    (let ((buffer-read-only nil))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer))

;; whitespace
(use-package whitespace
  :delight (whitespace-mode nil "whitespace")
  :config

  (setq whitespace-style '(face tabs)
        whitespace-line-column 79)

  (defun my-whitespace-column-regexp()
    (let ((line-column (or whitespace-line-column fill-column)))
      (format
       "^\\([^\t\n]\\{%s\\}\\|[^\t\n]\\{0,%s\\}\t\\)\\{%d\\}%s\\(.\\).*$"
       tab-width
       (1- tab-width)
       (/ line-column tab-width)
       (let ((rem (% line-column tab-width)))
         (if (zerop rem)
             ""
           (format ".\\{%d\\}" rem))))))

  (defun my-highlight-column ()
    (font-lock-add-keywords
     nil
     `((,(my-whitespace-column-regexp) 2 whitespace-line prepend))
     t))

  ;; Highlight whitespaces and long strings
  (defun my-highlight-whitespaces ()
    (when (highlight-prog)
      (cond
       ((fboundp 'whitespace-mode)
        (whitespace-mode 1)
        (setq-local show-trailing-whitespace t)
        ;; Show single column marker
        (my-highlight-column)))))

  (add-hook 'font-lock-mode-hook 'my-highlight-whitespaces))

(use-package recentf
  :bind (("C-x C-r" . recentf-open-files))
  :config
  (setq recentf-max-menu-items 50)
  (recentf-mode 1)
  (run-at-time nil (* 5 60) 'recentf-save-list))

;; Unique buffer names
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward))

;; Python-mode settings
(defvar py-flake8-history nil) ; workaround python-mode.el bug
(setq py-flake8-command-args
      '("--ignore=E12,F403" "--max-line-length=120" "--max-complexity=73"))

(defun my-find-py3 ()
  (when (not (boundp 'python3-executable))
    (set-executable 'python3-executable
                    '("taxi-python3" "python3.7" "python3"))))

(defun ipdb-insert-set-trace()
  (interactive)
  (insert "from pdb import set_trace; set_trace()"))

(use-package python
  :delight (python-mode "py" "python-mode")
  :bind (:map python-mode-map
              ("C-c b" . ipdb-insert-set-trace)))

(use-package python-black
  :after python
  :config
  (set-executable 'python-black-command
                  '("taxi-black" "black"))
  :bind (:map python-mode-map ("C-c f" . python-black-buffer)))

(use-package pyimpsort
  :after python
  :config
  ;; TODO: use own patched pyimpsort.py
  (setq pyimpsort-script
        (expand-file-name "scripts/pyimpsort.py" user-emacs-directory)))

;; C/C++ mode settings
(use-package google-c-style
  :after cc-mode
  :hook (c++-mode . (lambda ()
                      (google-set-c-style)
                      (c-set-offset 'inlambda 0))))

(use-package clang-format
  :after cc-mode
  :config
  (set-executable 'clang-format-executable
                  '("clang-format-7" "clang-format"))
  :bind (:map c++-mode-map ("C-c f" . clang-format-buffer)))

;; Lua
(use-package manual-indent
  :after lua-mode
  :hook
  (lua-mode-hook . (lambda ()
                     (setq lua-indent-level 4)
                     (manual-indent-mode 1)
                     (electric-indent-local-mode 0))))

(use-package flycheck
  :config
  (defun disable-flycheck-mode ()
    (when (fboundp 'flycheck-mode)
      (flycheck-mode 0)))

  ;; Enable flycheck globally
  (global-flycheck-mode)

  :hook
  (python-mode .
               (lambda ()
                 (my-find-py3)
                 (when python3-executable
                   (setq-local
                    flycheck-python-flake8-executable python3-executable
                    flycheck-python-pycompile-executable python3-executable
                    flycheck-python-pylint-executable python3-executable))))
  (c++-mode . (lambda ()
                (setq-local flycheck-gcc-language-standard "c++17"
                            flycheck-clang-language-standard "c++17")))
  (vterm-mode . disable-flycheck-mode)

  :delight (flycheck-mode "/flycheck" "flycheck"))

;; company
(use-package company
  :defer t
  :config
  (setq company-idle-delay 0.5)
  (setq company-selection-wrap-around t)
  ;;(company-tng-configure-default)
  )

;; lsp-mode setup
(use-package lsp-mode
  :defer t
  :delight (lsp-mode "/lsp" "lsp")
  :config
  ;; clangd
  (set-executable 'lsp-clients-clangd-executable
                  '("clangd-12" "clangd-11" "clangd-10" "clangd-9" "clangd"))
  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"
                                  "--header-insertion=never"))

  ;; pyls
  (set-executable 'vitja-lsp-pyls-server-command
                  '("/usr/lib/yandex/taxi-py3-2/bin/pyls" "pyls"))
  ;;(setq lsp-pyls-server-command vitja-lsp-pyls-server-command)
  (setq xref-prompt-for-identifier nil)
  (setq lsp-enable-links nil)

  ;;(lsp-headerline-breadcrumb-mode 0)
  ;;(add-hook 'lsp-after-initialize-hook 'lsp-headerline-breadcrumb-mode)
  ;; (add-hook
  ;;  'hack-local-variables-hook
  ;;  (lambda ()
  ;;    (when (boundp 'vitja-lsp-python-path)
  ;;      (setq-local
  ;;       lsp-pyls-server-command
  ;;       (list "/bin/sh" "-c"
  ;;             (concat
  ;;              "PYTHONPATH='"
  ;;              (string-join
  ;;               ;; filter out bad parts
  ;;               (seq-filter
  ;;                (lambda (string)
  ;;                  (if (string-match "['\"\\$:;]" string)
  ;;                      (progn
  ;;                        (message "Dangerous PYTHONPATH part %s" string) nil)
  ;;                    t))
  ;;                vitja-lsp-python-path)
  ;;               ":") "' " vitja-lsp-pyls-server-command))))))

  ;; golang
  (set-executable 'lsp-gopls-server-path '("gopls" "/home/vitja/go/bin/gopls"))

  ;; Set https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (setq lsp-enable-symbol-highlighting nil
        lsp-prefer-flymake nil
        lsp-enable-snippet nil
        lsp-headerline-breadcrumb-enable nil)

  ;;(setq lsp-headerline-breadcrumb-enable nil)

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
             ;; File mode specification error: (wrong-type-argument stringp (inhibit-mode-name-delight C++//l c++))
             ;;(c++-mode "c++" "cc-mode")
             )))

;; Go support
(use-package go-mode
  :defer t
  :hook
  (go-mode
   . (lambda ()
       (setq tab-width 4)
       (setq indent-tabs-mode 1)))
  :bind (:map go-mode-map
              ("C-c f" . gofmt)))

;; Dired setup
(use-package dired
  :config
  (add-hook 'dired-mode-hook
            (lambda ()
              (load "dired-x")
              (if window-system (hl-line-mode 1))
              (dired-omit-mode 1)))
  (setq dired-listing-switches "-alhv --group-directories-first"
        dired-isearch-filenames 'dwim))

;; ;; ido-mode
;; (use-package ido
;;   :config
;;   (setq ido-enable-flex-matching t
;;         ido-default-buffer-method 'selected-window
;;         ido-use-virtual-buffers t)
;;   (ido-mode 1))

;; ivy
(use-package ivy
  :delight
  :config
  (ivy-mode 1)
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  (setq ivy-display-style 'fancy)
  (setq ivy-use-selectable-prompt t)

  ;; Do not show "./" and "../" in the `counsel-find-file' completion list
  (setq ivy-extra-directories nil)    ;Default value: ("../" "./")
  )

(use-package ivy-hydra)

;; See https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-lisp/prot-ivy-deprecated-conf.el
(use-package counsel
  :config
  ;;(global-set-key (kbd "M-y") 'counsel-yank-pop)

  ;; see https://github.com/abo-abo/swiper/issues/1333#issuecomment-436960474
  (defun counsel-find-file-fallback-command ()
    "Fallback to non-counsel version of current command."
    (interactive)
    (when (bound-and-true-p ivy-mode)
      (ivy-mode -1)
      (add-hook 'minibuffer-setup-hook
                'counsel-find-file-fallback-command--enable-ivy))
    (ivy-set-action
     (lambda (current-path)
       (let ((old-default-directory default-directory))
         (let ((default-directory current-path))
           (call-interactively 'find-file))
         (setq default-directory old-default-directory))))
    (ivy-immediate-done))

  (defun counsel-find-file-fallback-command--enable-ivy ()
    (remove-hook 'minibuffer-setup-hook
                 'counsel-find-file-fallback-command--enable-ivy)
  (ivy-mode t))
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . ivy-switch-buffer)
         ("C-x d" . counsel-dired)
         ("C-x C-r" . counsel-recentf)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ;;("M-s r" . counsel-rg)
         ;;("M-s g" . counsel-git-grep)
         ;;("M-s l" . counsel-find-library)
         ;;("M-s z" . prot/counsel-fzf-rg-files)
         ;;:map ivy-minibuffer-map
         ;;("C-r" . counsel-minibuffer-history)
         ;;("s-y" . ivy-next-line)        ; Avoid 2× `counsel-yank-pop'
         ;;("C-SPC" . ivy-restrict-to-matches)))
         :map counsel-find-file-map
         ("C-f" . counsel-find-file-fallback-command)))

(use-package ivy-rich
  :ensure t
  :config
  (setq ivy-rich-path-style 'abbreviate)

  (setcdr (assq t ivy-format-functions-alist)
          #'ivy-format-function-line)
  :hook (after-init . ivy-rich-mode))

;; (use-package ivy-posframe
;;   :ensure t
;;   :delight
;;   :config
;;   (setq posframe-mouse-banish nil)
;;   ;; (setq ivy-posframe-parameters
;;   ;;       '((left-fringe . 2)
;;   ;;         (right-fringe . 2)
;;   ;;         (internal-border-width . 2)
;;   ;;         ;; (font . "Iosevka-10.75:hintstyle=hintfull")
;;   ;;         ))
;;   ;; (setq ivy-posframe-height-alist
;;   ;;       '((swiper . 15)
;;   ;;         (swiper-isearch . 15)
;;   ;;         (t . 10)))
;;   ;; (setq ivy-posframe-display-functions-alist
;;   ;;       '((complete-symbol . ivy-posframe-display-at-point)
;;   ;;         (swiper . nil)
;;   ;;         (swiper-isearch . nil)
;;   ;;         (t . ivy-posframe-display-at-frame-center)))
;;   :hook (after-init . ivy-posframe-mode))

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
  :delight (projectile-mode nil "projectile")
  :config
  ;;(add-to-list 'projectile-project-root-files-bottom-up ".arc")

  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (require 'ffap)

  :bind (("C-c p 1" . 'projectile-find-file-at-point)
         ("C-c p 0" . (lambda ()
                        (interactive)
                        (projectile--find-file-at-point nil 'find-file-other-window)))))
;; Magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package windmove
  :bind
  ([M-left]  . windmove-left)
  ([M-right] . windmove-right)
  ([M-up]    . windmove-up)
  ([M-down]  . windmove-down))

(use-package gud
  :bind
  ([f4]   . gud-gdb)
  ([f7]   . gud-step)
  ([f8]   . gud-next)
  ([C-f8] . gud-break))

(use-package org
  :defer t
  :config
  (setq org-export-with-smart-quotes t
        org-src-fontify-natively t
        org-startup-folded nil)
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (setq org-todo-keywords
        '((sequence
           "TODO" "INPROGRESS" "HOLD" "|" "DONE" "DELEGATED" "CANCELLED")))
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo))

(use-package vterm
  :defer t
  :bind
  (:map
   vterm-mode-map
   ([M-right] . nil)
   ([M-left]  . nil))
  :config
  (setq vterm-max-scrollback 100000)
  (setq vterm-timer-delay 0.02)
  (setq vterm-buffer-name-string "*vterm*: %s"))

(use-package multi-vterm
  :defer t
  :config
  (defun multi-vterm-switch-to-nth (n)
    (let ((vterm-buffer (nth n multi-vterm-buffer-list)))
      (when vterm-buffer
        (switch-to-buffer vterm-buffer))))

  :bind
  (("C-c t" . multi-vterm)
   :map
   vterm-mode-map
   ([C-prior] . multi-vterm-next)
   ([C-next]  . multi-vterm-next)
   ("M-1" . (lambda () (interactive) (multi-vterm-switch-to-nth 0)))
   ("M-2" . (lambda () (interactive) (multi-vterm-switch-to-nth 1)))
   ("M-3" . (lambda () (interactive) (multi-vterm-switch-to-nth 2)))
   ("M-4" . (lambda () (interactive) (multi-vterm-switch-to-nth 3)))
   ("M-5" . (lambda () (interactive) (multi-vterm-switch-to-nth 4)))
   ("M-6" . (lambda () (interactive) (multi-vterm-switch-to-nth 5)))
   ("M-7" . (lambda () (interactive) (multi-vterm-switch-to-nth 6)))
   ("M-8" . (lambda () (interactive) (multi-vterm-switch-to-nth 7)))
   ("M-9" . (lambda () (interactive) (multi-vterm-switch-to-nth 8)))))

;; Emacs server
(use-package server
  :defer 1
  :config
  (if window-system (server-start)))

(use-package vc
  :config
  ;; Only load vc-arc when available
  (when (require 'vc-arc nil t)
    (add-to-list 'vc-handled-backends 'arc)))

(use-package gif-screencast
  :defer t
  :config
  (define-key gif-screencast-mode-map (kbd "<f8>") 'gif-screencast-toggle-pause)
  (define-key gif-screencast-mode-map (kbd "<f9>") 'gif-screencast-stop))

(use-package winner
  :config
  (winner-mode 1))

;; Custom functions
(defun projectile--find-file-at-point (invalidate-cache &optional ff-variant)
  "Jump to a project's file at point.
With INVALIDATE-CACHE invalidates the cache first.  With FF-VARIANT set to a
defun, use that instead of `find-file'.   A typical example of such a defun
would be `find-file-other-window' or `find-file-other-frame'"
  (interactive "P")
  (projectile-maybe-invalidate-cache invalidate-cache)
  (let* ((project-root (projectile-ensure-project (projectile-project-root)))
         (file (or (ffap-file-at-point)
                   (thing-at-point 'filename)
                   (thing-at-point 'symbol)
                   (read-string "No file name at point. Please provide file name:")))
         (ff (or ff-variant #'find-file)))
    (when file
      (funcall ff (expand-file-name file project-root))
      (run-hooks 'projectile-find-file-hook))))

(defun projectile-find-file-at-point(&optional invalidate-cache)
  "Jump to a project's file at point.
With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (interactive "P")
  (projectile--find-file-at-point invalidate-cache))

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

(use-package my
  :config
  ;; Grep
  (setq grep-command "grep -Eni ")
  ;; Compilation
  (setq compilation-environment '("LC_ALL=C" "TERM=ansi")
        compile-command "make ")
  :bind
  (("<f9>"  . my-compile)
   ("<f12>" . my-grep)))

(use-package telega
  :defer t
  :config
  (setq telega-emoji-company-backend 'telega-company-emoji)
  (setq telega-symbol-telegram "tg:")
  (setq telega-symbol-mode "")
  (setq telega-completing-read-function 'ivy-completing-read)
  (setq telega-chat-input-complete-function 'counsel-company)
  (defun my-telega-chat-mode ()
    (set (make-local-variable 'company-backends)
         (append (list telega-emoji-company-backend
                       'telega-company-username
                       'telega-company-hashtag)
                 (when (telega-chat-bot-p telega-chatbuf--chat)
                   '(telega-company-botcmd))))
    (company-mode 1))
  :bind-keymap
  (("C-c e" . telega-prefix-map))
  :delight
  (telega-chat-mode "Chat" "chat")
  :hook
  ((telega-chat-mode . my-telega-chat-mode)))

(use-package emacs
  :bind
  (("C-c #"   . comment-region)
   ("C-x C-c" . delete-frame-or-kill-emacs)
   ;; window control
   ([C-prior] . previous-buffer)
   ([C-next]  . next-buffer)) )

(if window-system
    (progn
      (global-set-key "\C-z" 'ignore)
      (global-set-key [mouse-4] 'scroll-down)
      (global-set-key [mouse-5] 'scroll-up)
      (global-set-key [mouse-2] 'nop)
      (global-set-key [C-f9] 'recompile)))

;; Load machine local configuration (if available)
(load (expand-file-name "local.el" user-emacs-directory) t)

(message (format "gcs-done: %d" gcs-done))
(message (format "Emacs init time: %s" (emacs-init-time)))

(provide 'init)
;;; init.el ends here
