(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

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
 '(global-font-lock-mode t nil (font-lock))
 '(inhibit-startup-screen t)
 '(load-home-init-file t t)
 '(menu-bar-mode nil)
 '(safe-local-variable-values (quote ((indent-tabs-mode . f) (sgml-indent-step . 1) (sgml-indent-data . 1))))
 '(save-place t nil (saveplace))
 '(setq font-lock-maximum-decoration t)
 '(show-paren-mode t nil (paren))
 '(line-number-mode t)
 '(x-select-enable-clipboard t))

;;(setq blink-cursor-mode 0)
(blink-cursor-mode 0)
;; (transient-mark-mode)

(if window-system
    (global-hl-line-mode))

(setq scroll-conservatively 50)
(setq scroll-up-agressively 0)
(setq scroll-down-agressively 0)
(setq visible-bell t)
(setq c-basic-offset 4)
;;(setq truncate-lines nil)
(setq use-dialog-box nil)

(show-paren-mode 1)

(require 'color-theme)
(if (fboundp 'color-theme-initialize)
    (color-theme-initialize))
(color-theme-dark-laptop)

(global-set-key [f9] (quote compile))
(global-set-key (quote [f2]) (quote save-buffer))
(global-set-key (quote [f3]) (quote find-file))
(global-set-key (quote [f4]) (quote gdb))
(global-set-key (quote [f7]) (quote gud-step))
(global-set-key (quote [f8]) (quote gud-next))
(global-set-key (quote [C-f8]) (quote gud-break))
(global-set-key (quote [f5]) (quote goto-line))

(global-set-key (quote [f12]) (quote grep))
(setq grep-command "grep -Eni ")
(setq kill-whole-line t)

;;(define-key ctl-x-map "p" 'previous-error)
;;(define-key ctl-x-map "n" 'next-error)
;;(define-key ctl-x-map "\C-p" 'previous-error)
;;(define-key ctl-x-map "\C-n" 'next-error)

(if window-system
    (progn
      (global-set-key "\C-z" (quote ignore))
      (global-set-key (quote [mouse-4]) (quote scroll-down))
      (global-set-key (quote [mouse-5]) (quote scroll-up))
      (global-set-key (quote [mouse-2]) (quote yank))
      (global-set-key (quote [mouse-3]) (quote yank))
      (global-set-key (quote [mouse-1]) '(copy-region-as-kill))
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


(setq auto-mode-alist (cons '(".[ch]$" . default-c-mode)
                auto-mode-alist))

;;(set-default-font "koi9x15")
;;(set-face-font "9x15")
;;;(x-handle-switch "-font" "9x15")

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq default-input-method "russian-computer")
(setq display-time-24hr-format t)
(display-time)
;;(display-battery)

(setq py-smart-indentation nil)

(defun silent-save-buffers-kill-emacs (&optional arg)
  "Offer to save each buffer, then kill this Emacs process.
   With prefix arg, silently save all file-visiting buffers, then kill."
  (interactive "P")
  (save-some-buffers arg t)
  (kill-emacs))

(defun delete-frame-or-kill-emacs (&optional arg)
  (interactive "P")
  (if (> (length (frame-list)) 1)
      (delete-frame)
    (progn
      (save-some-buffers arg t)
      (kill-emacs))))

;;(define-key ctl-x-map "\C-c" 'silent-save-buffers-kill-emacs)
(define-key ctl-x-map "\C-c" 'delete-frame-or-kill-emacs)

(global-set-key [C-prior] 'cyclebuffer-backward)
(global-set-key [C-next] 'cyclebuffer-forward)


(setq compilation-environment '("LC_ALL=C"))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default py-indent-offset 4)

(require 'whitespace)

(setq whitespace-style (quote
                        (tabs tab-mark trailing)))
(global-whitespace-mode)
;;(autoload 'whitespace-mode           "whitespace" "Toggle whitespace visualization."        t)
;;(autoload 'whitespace-toggle-options "whitespace" "Toggle local `whitespace-mode' options." t)

(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
(setq linum-format (if window-system "%4d" "%4d "))
(global-linum-mode)

;; mcedit-alike word movements functions
(setq word "A-Za-z0-9")
(setq delims " \t\n")
(setq specials "^A-Za-z0-9 \t\n")

(defun mc-forward-word (count)
  (cond
   ((not (= 0 (skip-chars-forward delims))) 1)
   ((not (= 0 (skip-chars-forward word))) (skip-chars-forward delims))
   ((skip-chars-forward specials) (skip-chars-forward delims))))

(defun mc-backward-word (count)
  (skip-chars-backward delims)
  (cond
   ((not (= 0 (skip-chars-backward word))) 2)
   ((skip-chars-backward specials) 0)))

(defun forward-word (count)
  (interactive "p")
  (if (> count 0)
      (mc-forward-word count)
    (mc-backward-word count)))

(defun backward-word (arg)
  "Move backward until encountering the beginning of a word.
   With argument, do this that many times."
  (interactive "p")
  (forward-word (- arg)))

(defun kill-word (arg)
  (interactive "p")
  (kill-region (point) (progn (forward-word arg) (point))))

(defun backward-kill-word (arg)
  (interactive "p")
  (kill-word (- arg)))


(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

