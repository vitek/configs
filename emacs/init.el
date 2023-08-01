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

;; Enlarge process buffer
(setq read-process-output-max (* 1024 256))

(when (string-equal (getenv "EMACS_DEBUG_STARTUP") "1")
  (setq use-package-verbose t)
  (setq use-package-minimum-reported-time 0.005)

  (defun my-message-with-timestamp (old-func fmt-string &rest args)
    "Prepend current timestamp (with microsecond precision) to a message"
    (apply old-func
           (concat (format-time-string "[%F %T.%3N %Z] ")
                   fmt-string)
           args))
  (advice-add 'message :around #'my-message-with-timestamp))

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(require 'cl-lib)
(require 'package)
(require 'seq)

;; Interface decorations
(menu-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

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

 jit-lock-defer-time 0.01

 ;; Localization settings
 default-input-method "russian-computer"
 current-language-environment "UTF-8"

 ;; Search
 case-fold-search t

 ;; Edit
 kill-whole-line t
 require-final-newline t

 ;; Scrolling
 scroll-preserve-screen-position 'always
 scroll-margin 1
 mouse-wheel-scroll-amount '(1
                             ((shift) . 1)
                             ((meta))
                             ((control) . text-scale))
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
(setq-default fill-column 80)

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

;; https://www.reddit.com/r/emacs/comments/y92y4b
(setq remote-file-name-inhibit-locks t)

(setq log-edit-hook (quote ()))

(custom-set-faces
;; '(whitespace-trailing ((t (:background "Red"))) 'now)
;; '(whitespace-tab ((t (:background "#433" :inverse-video nil))) 'now)
;; '(whitespace-line ((t (:background "gray"))) 'now)
 '(aw-leading-char-face ((t (:height 1.0))) 'now))

(defun set-executable (key choices)
  (let ((value (seq-find 'executable-find choices)))
    (when value
      (message (format "Executable %s chosen from %s" value choices))
      (set key value))))

;; Load custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; Don't initialize later as well
(setq package-enable-at-startup nil)

;; Configure package
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("stable.melpa" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")))

(setq package-archive-priorities
      '(("stable.melpa" . 10)
         ("gnu"          . 10)
         ("melpa"        . 10)))

(when (< emacs-major-version 27)
  (package-initialize))

;; Install binary dependencies
(let ((build-timestamp-dir
       (expand-file-name ".build-deps/" user-emacs-directory))
      (do-build-deps :ask))
  (defmacro build-action (name body)
    `(let ((build-timestamp (concat build-timestamp-dir (symbol-name ,name))))
       (when (not (file-exists-p build-timestamp))
         (when (if (eq do-build-deps :ask)
                   (setq do-build-deps (yes-or-no-p "Build dependencies? "))
                 do-build-deps)
           (message (format "Performing build action [%s]..." ,name))
           (progn ,body)
           (with-temp-buffer (write-file build-timestamp))))))
  (make-directory build-timestamp-dir :parents)
  ;; (build-action 'vterm
  ;;               (vterm-module-compile))
  (build-action 'all-the-icons
                (all-the-icons-install-fonts t)))

(defun derived-mode-parents (mode)
  (and mode
       (cons mode (derived-mode-parents
                   (get mode 'derived-mode-parent)))))

(defun highlight-prog ()
  (cl-intersection (derived-mode-parents major-mode)
                '(prog-mode text-mode cmake-mode)))

;; Color theme
;;(load-theme 'zenburn t)
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

(use-package git-grep
  :commands (git-grep)
  :defer t)

(use-package rg
  :commands (rg))

(use-package yaml-xref
  :config
  (setq yaml-xref-exe (expand-file-name "scripts/yaml-xref" user-emacs-directory))
  :hook
  (yaml-mode . (lambda ()
                 (add-hook 'xref-backend-functions #'yaml-xref-backend nil t))))

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
  :config
  (global-mc-move-mode)
  :bind
  (("M-f"   . mc-move-forward-word)
   ("M-b"   . mc-move-backward-word)
   ("M-d"   . mc-move-kill-word)
   ("M-DEL" . mc-move-backward-kill-word)
   ("M-@"   . mc-move-mark-word)))

(use-package tools
  :commands (insert-date insert-python-datetime insert-rfc-datetime)
  :bind
  (("C-c f"   . window-config-toggle)

   ("C-c M-u" . underscore-uppercase)
   ("C-c M-l" . underscore-lowercase)
   ("C-c M-c" . camelcase)))

(use-package ansi-color
  :defer t
  :config
  (setq ansi-color-names-vector
        ["#555555" "#CC9393" "#7F9F7F" "#F0DFAF"
         "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
  (defun my/ansi-colorize-buffer ()
    (let ((buffer-read-only nil))
      (ansi-color-apply-on-region (point-min) (point-max))))

  :hook
  ((compilation-filter . my/ansi-colorize-buffer)))

(use-package term
  :defer t
  :after ansi-color
  :config
  (seq-map-indexed
   (lambda (color idx)
     (let ((color-name (elt ansi-term-color-vector (1+ idx))))
       (custom-set-faces
        `(,color-name
          ((t (:foreground ,color :background ,color))) 'now))))
   ansi-color-names-vector))

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
        (display-fill-column-indicator-mode 1)
        (setq-local show-trailing-whitespace t)
        ;; Show single column marker
        ;;(my-highlight-column)
        ))))
  :hook
  ((font-lock-mode . my-highlight-whitespaces)))

(use-package recentf
  :bind (("C-x C-r" . recentf-open-files))
  :config
  (setq recentf-max-menu-items 100)
  (recentf-mode 1)
  (run-at-time nil (* 5 60)
               (lambda ()
                 (let ((inhibit-message t))
                   (recentf-save-list)))))

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
  :hook
  (python-mode . (lambda ()
                   (setq-local fill-column 79)))
  :bind (:map python-mode-map
              ("C-c b" . ipdb-insert-set-trace)))

(use-package elpy
  :ensure t
  :config
  (delete 'elpy-module-highlight-indentation elpy-modules)
  (delete 'elpy-module-django elpy-modules)
  :hook
  (python-mode . elpy-enable))

(use-package python-black
  :after python
  :config
  (set-executable 'python-black-command
                  '("my-taxi-black" "taxi-black" "black"))
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
  :ensure t
  :commands flycheck-mode
  :hook
  (prog-mode . flycheck-mode)
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

  :delight (flycheck-mode "/flycheck" "flycheck"))

(use-package flyspell
  :hook (org-mode . flyspell-mode)
  :config
  (defconst my-spell-dictionaries '("ru" "english"))
  (setq ispell-dictionary (car my-spell-dictionaries))

  :bind
  (([f5]   .
    (lambda ()
      (interactive)
      (let ((current-language (or ispell-local-dictionary ispell-dictionary)))
        (ispell-change-dictionary
         (car (or (cdr (member current-language my-spell-dictionaries))
                  my-spell-dictionaries))))))))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy
  :after flyspell-correct
  :config
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

;; ;; company
;; (use-package company
;;   :ensure t
;;   :commands (global-company-mode company-mode company-complete-common)
;;   :config
;;   (setq company-idle-delay 0.5)
;;   (setq company-selection-wrap-around t)
;;   (set-executable 'company-clang-executable
;;                   '("clang-12" "clang-11" "clang-10" "clang-7"))
;;   ;;(company-tng-configure-default)
;;   )

(use-package orderless
  :custom
  (completion-styles '(orderless))      ; Use orderless
  (completion-category-defaults nil)    ; I want to be in control!
  (completion-category-overrides
   '((file (styles basic-remote ; For `tramp' hostname completion with `vertico'
                   orderless))))

  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     ;; orderless-flex                       ; Basically fuzzy finding
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal          ; Recommended for dispatches instead
   )))


(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-exclude-modes'.
  :init
  (global-corfu-mode)

  :config
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active) ; Useful if I ever use MCT
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil)       ; Ensure auto completion is disabled
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  ;; Setup lsp to use corfu for lsp completion
  (defun kb/corfu-setup-lsp ()
    "Use orderless completion style with lsp-capf instead of the
default lsp-passthrough."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))))


;; lsp-mode setup
(use-package lsp-mode
  :defer t
  :delight (lsp-mode "/lsp" "lsp")
  :config

  (defun my-lsp-force-reconnect ()
    (interactive)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and lsp-mode
                   (member major-mode '(c-mode cc-mode)))
          (lsp-disconnect)
          (lsp +1)))))

  ;; clangd
  (set-executable 'lsp-clients-clangd-executable
                  '("clangd-chooser" "clangd-13" "clangd-12"
                    "clangd-11" "clangd-10" "clangd-9" "clangd"))
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
  :hook ((c++-mode    . lsp-deferred)
         (c-mode      . lsp-deferred)
         (go-mode     . lsp-deferred)))

(use-package lsp-java
  :config
  (setq lsp-java-java-path "/home/vitja/arcadia/taxi/mj/services/udp-for-market/jdk/bin/java")
  (setenv "JAVA_HOME" "/home/vitja/arcadia/taxi/mj/services/udp-for-market/jdk")
  (setenv "CLASSPATH" "/home/vitja/arcadia/taxi/mj/services/udp-for-market/idea")
  :hook ((java-mode    . lsp-deferred)))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs
        '("~/nda/emacs/snippets"))
  (yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.
)

;; delight, tune minor mode bar
(use-package delight
  :config

  ;; https://www.emacswiki.org/emacs/DelightedModes
  (advice-add 'c-update-modeline :override #'ignore)

  (delight '((abbrev-mode nil "abbrev")
             (company-mode nil "company")
             (eldoc-mode nil "eldoc")
             (yas-minor-mode nil "yasnippet")
             ;; File mode specification error: (wrong-type-argument stringp (inhibit-mode-name-delight C++//l c++))
             (c++-mode "C++" "cc-mode")
             (lsp-mode "/lsp" "lsp-mode")
             )))

;; Go support
(use-package go-mode
  :hook
  (go-mode
   . (lambda ()
       (setq tab-width 4)
       (setq indent-tabs-mode 1)))
  :bind (:map go-mode-map
              ("C-c f" . gofmt)))

;; Dired setup
(use-package dired
  :commands (dired)
  :config
  (setq dired-listing-switches "-alhv --group-directories-first"
        dired-isearch-filenames 'dwim)
  (defun my-dired-mode-hook ()
    (load "dired-x")
    (when window-system (hl-line-mode 1))
    (dired-omit-mode 1))
  :hook
  ((dired-mode . my-dired-mode-hook)))

;; ;; ido-mode
;; (use-package ido
;;   :config
;;   (setq ido-enable-flex-matching t
;;         ido-default-buffer-method 'selected-window
;;         ido-use-virtual-buffers t)
;;   (ido-mode 1))

;; ivy
(use-package ivy
  :defer t
  :delight
  :config
  (ivy-mode 1)
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-re-builders-alist
        '(;;(counsel-find-file . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  ;; (setq ivy-initial-inputs-alist
  ;;       `((counsel-find-file . "^")
  ;;         ,@ivy-initial-inputs-alist))
  (setq ivy-display-style 'fancy)
  (setq ivy-use-selectable-prompt t)
  (setq ivy--recompute-index-inhibit t)

  ;; Do not show "./" and "../" in the `counsel-find-file' completion list
  (setq ivy-extra-directories nil)    ;Default value: ("../" "./")
  )

(use-package ivy-hydra
  :defer t)

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

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :ensure t
  :init
  (setq ivy-rich-path-style 'abbreviate)

  (setcdr (assq t ivy-format-functions-alist)
          #'ivy-format-function-line)
  (ivy-rich-mode 1))

(use-package ivy-pass
  :ensure t
  :bind
  ("C-c C-p" . ivy-pass))

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

  ;; To slow on arc tree
  (advice-remove
   'compilation-find-file
   #'compilation-find-file-projectile-find-compilation-buffer)

  :bind (("C-c p 1" . 'projectile-find-file-at-point)
         ("C-c p 0" . (lambda ()
                        (interactive)
                        (projectile--find-file-at-point nil 'find-file-other-window)))))
;; Magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))

(use-package ace-window
  :config
  (setq aw-scope 'frame)
  :bind
  ("M-o"     . ace-window))

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
  ;; Disable box around checkbox causing screen flickering
  (defvar my-org-agenda-map (make-sparse-keymap))
  (custom-set-faces
   '(org-checkbox ((t (:box nil :inherit org-checkbox)))))
  (setq org-export-with-smart-quotes t
        org-src-fontify-natively t
        org-startup-folded nil)
  (set-face-underline 'org-ellipsis nil)
  (setq org-ellipsis "…")
  (setq org-log-done 'time)
  (setq org-adapt-indentation t)
  (setq org-default-notes-file (expand-file-name "~/nda/org/notes.org"))

  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (setq org-todo-keywords
        '((sequence
           "TODO(t)" "INPROGRESS(p)" "HOLD(h)" "|"
           "DONE(d)" "DELEGATED(D)" "CANCELLED(C)")))

  :bind (:map my-org-agenda-map
              ("a" . org-agenda-list)
              ("t" . org-todo-list))
  :bind-keymap
  ("C-c a" . my-org-agenda-map)

  :hook
  ((org-mode . (lambda ()
                 (setq-local fill-column 90)
                 (visual-fill-column-mode 1)
                 (visual-line-mode 1)
                 (when (> emacs-major-version 27)
                   (org-bullets-mode 1))))
   (org-agenda-mode . (lambda ()
                        (when window-system (hl-line-mode 1))))
   (org-after-todo-statistics . org-summary-todo)))

(use-package pdf-tools
  :hook (doc-view-mode . pdf-tools-install))

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

(use-package vc-hooks
  :config
  ;; Only load vc-arc when available
  (remove-hook 'find-file-hooks 'vc-find-file-hook)
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

(use-package my
  :config
  ;; Grep
  (setq grep-command "grep -Eni ")
  ;; Compilation
  (setq compilation-environment '("LC_ALL=C" "TERM=ansi")
        compile-command "make ")
  (setq compilation-scroll-output 'first-error)

  :bind
  (("<f9>"  . my-compile)
   ("<f12>" . my-grep)))

;; (use-package telega
;;   :defer t
;;   :config
;;   (setq telega-emoji-company-backend 'telega-company-emoji)
;;   (setq telega-symbol-telegram "tg:")
;;   (setq telega-symbol-mode "")
;;   ;;(setq telega-symbol-reply "<-")
;;   (setq telega-completing-read-function 'ivy-completing-read)
;;   (setq telega-chat-input-complete-function 'counsel-company)
;;   (add-to-list 'telega-symbols-emojify 'reply)
;;   (defun my-telega-chat-mode ()
;;     (set (make-local-variable 'company-backends)
;;          (append (list telega-emoji-company-backend
;;                        'telega-company-username
;;                        'telega-company-hashtag)
;;                  (when (telega-chat-bot-p telega-chatbuf--chat)
;;                    '(telega-company-botcmd))))
;;     (company-mode 1))
;;   :bind-keymap
;;   (("C-c e" . telega-prefix-map))
;;   :bind
;;   (:map telega-msg-button-map
;;         ("к" . telega-msg-reply)
;;         ("у" . telega-msg-edit))
;;   :delight
;;   (telega-chat-mode "Chat" "chat")
;;   :hook
;;   ((telega-chat-mode . my-telega-chat-mode)))

(use-package which-key
  :delight (which-key-mode nil "whitespace")
  :config
  (setq which-key-idle-delay 1.5)
  (which-key-setup-side-window-right-bottom)
  ;;(which-key-setup-minibuffer)
  (which-key-mode))

(use-package emacs
  :bind
  (("C-c #"   . comment-region)
   ("C-x C-c" . delete-frame-or-kill-emacs)
   ("C-c w"   . browse-url-at-point)
   ;; window control
   ([C-prior] . previous-buffer)
   ([C-next]  . next-buffer)
   ([Back] . previous-buffer)
   ([Forward]  . next-buffer)
   ("M-<f5>"  . modus-themes-toggle))
  :config

  (when window-system
      (dolist (mode '(prog-mode-hook text-mode-hook cmake-mode-hook))
        (add-hook mode #'hl-line-mode)))

  ;; (eq (frame-parameter nil 'fullscreen) 'fullboth)

  (setq mode-line-position-column-line-format '(" %l:%c"))

  (when window-system
    (setq modus-themes-common-palette-overrides
          `(
            ;; From the section "Make the mode line borderless"
            (border-mode-line-active unspecified)
            (border-mode-line-inactive unspecified)))
    (setq modus-themes-to-toggle '(modus-vivendi-tinted
                                   modus-operandi-tinted))
    (load-theme (car modus-themes-to-toggle) t))
)

(use-package goto-addr
  :config
  (setq goto-address-url-face 'underline)
  ;; :bind
  ;; (:map goto-address-highlight-keymap
  ;;       ("C-c C-o" . goto-address-at-point))
  :hook
  ((vterm-mode eshell-mode shell-mode) . goto-address-mode)
  (prog-mode . goto-address-prog-mode))

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

(use-package eglot
  :defer t
  :config
  (setq eglot-extend-to-xref  t))

(use-package telephone-line
  :config
  (telephone-line-mode 1))

;; (use-package nano-modeline
;;   :config
;;   (setq nano-modeline-position 'bottom
;;         nano-modeline-space-top 0.0
;;         nano-modeline-space-bottom 0.0))

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
