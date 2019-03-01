(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(case-fold-search t)
 '(column-number-mode t)
 '(compile-command "make ")
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(display-time-mode t)
 '(frame-title-format "emacs: %b")
 '(global-font-lock-mode t nil (font-lock))
 '(inhibit-startup-screen t)
 '(line-number-mode t)
 '(load-home-init-file t t)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(safe-local-variable-values
   (quote ((indent-tabs-mode . f)
           (sgml-indent-step . 1)
           (sgml-indent-data . 1))))
 '(save-place t nil (saveplace))
 '(setq font-lock-maximum-decoration t)
 '(show-paren-mode t nil (paren))
 '(x-select-enable-clipboard t)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(log-edit-hook (quote ())))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(diff-added ((t (:foreground "Green"))) 'now)
 '(diff-removed ((t (:foreground "Red"))) 'now)

 '(show-ws-trailing-whitespace ((t (:background "Red"))) 'now)
 '(show-ws-tab ((t (:background "#222"))) 'now))

(setq custom-file "~/.emacs-custom.el")
(load custom-file t)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(defun install-my-packages()
  (interactive)
  (progn
    (package-refresh-contents)
    (package-initialize)
    ;; list of packages to install
    (package-install 'clang-format)
    (package-install 'cmake-ide)
    (package-install 'cmake-mode)
    (package-install 'color-theme)
    (package-install 'company)
    (package-install 'company-irony)
    (package-install 'flycheck)
    (package-install 'helm)
    (package-install 'irony)
    (package-install 'jedi)
    (package-install 'lua-mode)
    (package-install 'rtags)
    (package-install 'zoom-frm)))

(require 'git-grep nil t)
(require 'jedi nil t)
(require 'irony nil t)
(require 'company nil t)
(require 'google-c-style nil t)
(require 'show-wspace nil t)
(require 'column-marker nil t)
(require 'yaml-mode nil t)
(require 'find-file-in-project nil t)
(require 'mc-move)

(setq jit-lock-defer-time 0.05)

;;(setq blink-cursor-mode 0)
(setq scroll-conservatively 50)
(setq scroll-up-agressively 0)
(setq scroll-down-agressively 0)
(setq visible-bell t)
(setq c-basic-offset 4)
(c-set-offset 'arglist-intro '+)
(c-set-offset 'inextern-lang 0)
;;(setq truncate-lines nil)
(setq use-dialog-box nil)
(setq kill-whole-line t)
(setq default-input-method "russian-computer")
(setq display-time-24hr-format t)
(setq grep-command "grep -Eni ")
(setq compilation-environment '("LC_ALL=C"))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq-default py-indent-offset 4)
(setq-default sgml-basic-offset 2)
(setq py-smart-indentation nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(show-paren-mode t)
(global-auto-revert-mode)
(blink-cursor-mode 0)
(display-time)
;;(display-battery)
;;(transient-mark-mode)
;;(if window-system (global-hl-line-mode))

;; Color theme setup
(when (require 'color-theme nil t)
  (progn (when (fboundp 'color-theme-initialize)
           (color-theme-initialize))
         (color-theme-dark-laptop)))
;;(require 'github-theme)

;; (when (require 'semantic nil t)
;;   (progn
;;     (global-semanticdb-minor-mode 1)
;;     (semantic-mode 1)
;;     (global-set-key "\M-n" 'semantic-complete-jump)
;;     (global-set-key (kbd "C-c r") 'semantic-symref)))


(defun my-compile()
  (interactive)
  (if (fboundp 'cmake-ide-compile)
      (call-interactively 'cmake-ide-compile)
    (call-interactively 'compile)))


;;(global-set-key "\C-s" 'isearch-forward-regexp)
;;(global-set-key "\C-r" 'isearch-backward-regexp)

(global-set-key [f9] 'my-compile)
(global-set-key (quote [f2]) 'save-buffer)
(global-set-key (quote [f3]) 'find-file)
(global-set-key (quote [f4]) 'gud-gdb)
(global-set-key (quote [f7]) 'gud-step)
(global-set-key (quote [f8]) 'gud-next)
(global-set-key (quote [C-f8]) 'gud-break)
(global-set-key (quote [f5]) 'goto-line)

(global-set-key (quote [f12]) 'grep)

;;(define-key ctl-x-map "p" 'previous-error)
;;(define-key ctl-x-map "n" 'next-error)
;;(define-key ctl-x-map "\C-p" 'previous-error)
;;(define-key ctl-x-map "\C-n" 'next-error)

(when
    (require 'zoom-frm nil t)
  (progn (global-set-key (kbd "C-M-=") 'zoom-in)
         (global-set-key (kbd "C-M--") 'zoom-out)
         (global-set-key (kbd "C-M-0") 'zoom-frm-unzoom)
         ;; mouse bindings
         (global-set-key (kbd "<C-mouse-4>") 'zoom-in)
         (global-set-key (kbd "<C-mouse-5>") 'zoom-out)))

(defun nop()
  (interactive))

(if window-system
    (progn
      (global-set-key "\C-z" 'ignore)
      (global-set-key [mouse-4] 'scroll-down)
      (global-set-key [mouse-5] 'scroll-up)
      (global-set-key [mouse-2] 'nop)
      (global-set-key [C-f9] 'recompile)))

(global-set-key (quote [f1] )
        (lambda ()
          (interactive)
          (let ((woman-topic-at-point t))
            (woman))))

(autoload 'woman "woman"
  "Decode and browse a UN*X man page." t)
(autoload 'woman-find-file "woman"
  "Find, decode and browse a specific UN*X man-page file." t)

(setq woman-cache-filename "~/.woman-cache")

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

(defun silent-save-buffers-kill-emacs (&optional arg)
  "Offer to save each buffer, then kill this Emacs process.
   With prefix ARG, silently save all file-visiting buffers, then kill."
  (interactive "P")
  (save-some-buffers arg t)
  (kill-emacs))

(defun delete-frame-or-kill-emacs (&optional arg)
  "Delete frame or kill emacs if current frame is the last one."
  (interactive "P")
  (if (> (length (frame-list)) 1)
      (delete-frame)
    (progn
      (save-some-buffers arg t)
      (kill-emacs))))

;;(define-key ctl-x-map "\C-c" 'silent-save-buffers-kill-emacs)
(define-key ctl-x-map "\C-c" 'delete-frame-or-kill-emacs)

(global-set-key [C-prior] 'previous-buffer)
(global-set-key [C-next] 'next-buffer)
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)


(defun derived-mode-parents (mode)
  (and mode
    (cons mode (derived-mode-parents
          (get mode 'derived-mode-parent)))))

;; Highlight whitespaces and long strings
(defun highlight-whitespaces ()
  (if (intersection (derived-mode-parents major-mode)
                    '(prog-mode text-mode cmake-mode))
      (progn
        (when (fboundp 'show-ws-highlight-trailing-whitespace)
          (progn
            (show-ws-highlight-trailing-whitespace)
            (show-ws-highlight-tabs)))
        ;; Show marker at 80 column
        (when
            (fboundp 'column-marker-1)
          (column-marker-1 79)))))
(add-hook 'font-lock-mode-hook 'highlight-whitespaces)

(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
(setq linum-format (if window-system "%4d" "%4d "))
;;(global-linum-mode)

(global-set-key (kbd "C-c #") 'comment-region)

;; Python-mode settings
(defun ipdb-insert-set-trace()
  (interactive)
  (insert "from ipdb import set_trace; set_trace()"))

(defvar py-flake8-history nil) ; workaround python-mode.el bug
(setq py-flake8-command-args
      '("--ignore=E12,F403" "--max-line-length=120" "--max-complexity=73"))
(setq jedi:complete-on-dot t)

(add-hook
 'python-mode-hook
 (lambda ()
   (progn
     (global-set-key (kbd "C-c b") 'ipdb-insert-set-trace)
     (jedi:setup))))

;; C/C++ mode settings
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)

(add-hook
 'c++-mode-hook
 (lambda ()
   (progn
     (setq flycheck-gcc-language-standard "c++11")
     (setq flycheck-clang-language-standard "c++11")
     (google-set-c-style))))

(add-hook 'after-init-hook 'global-company-mode)
;;(eval-after-load 'company
;;  '(add-to-list 'company-backends 'company-irony))

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)


;; Enable flycheck globally
(add-hook 'after-init-hook
          (lambda ()
            (when (boundp 'global-flycheck-mode)
              (global-flycheck-mode))))

;(require 'flycheck-irony)
;;a
;;(eval-after-load 'flycheck
;;  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
;;(flycheck-irony-setup flycheck-mode-set-explicitly)

;; (require 'rtags)
;; (require 'company-rtags)
;; (require 'rtags-helm)

;; (setq rtags-completions-enabled t)
;; (eval-after-load 'company
;;   '(add-to-list
;;     'company-backends 'company-rtags))
;; (setq rtags-autostart-diagnostics t)
;; (rtags-enable-standard-keybindings)

;; (setq rtags-use-helm t)

;; (rtags-start-process-unless-running)

(require 'manual-indent)

(add-hook 'lua-mode-hook
          (lambda ()
            (setq lua-indent-level 4)
            (manual-indent-mode 1)
            (electric-indent-local-mode 0)))

(provide '.emacs)
;;; .emacs ends here
