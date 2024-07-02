;;; vitja-vertico.el --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package all-the-icons)

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; (use-package nerd-icons-completion
;;   :after marginalia
;;   :config
;;   (nerd-icons-completion-mode)
;;   (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))


(use-package marginalia
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'center)
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Enable vertico
(use-package vertico
  :custom
  (vertico-count 13)                    ; Number of candidates to display
  (vertico-resize 'grow-only)
  (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?

  :hook
  (minibuffer-setup . vertico-repeat-save) ; Make sure vertico state is saved

  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

(defun my/vertico-directory-tilde()
  (interactive)
  (delete-minibuffer-contents)
  (insert "~/"))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)
              ("C-j" . (lambda() (interactive) (vertico-exit t))
              ("~" . my/vertico-directory-tilde))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-`" . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :config

  (defun my/consult-command (command &rest args)
    (setq this-command command)
    (apply command args))

  (defun my/consult-ripgrep-restart (dir-func)
    (let
        ((saved-default-directory default-directory)
         (input (ignore-errors
                  (buffer-substring-no-properties
                   (1+ (minibuffer-prompt-end)) (point-max)))))
      (run-at-time 0 nil
                   (lambda ()
                     (let* ((default-directory saved-default-directory)
                            (default-directory (funcall dir-func)))
                       (my/consult-command
                        'consult-ripgrep default-directory input)))))
    (minibuffer-quit-recursive-edit))

  (defun my/consult-ripgrep-up-directory ()
    (interactive)
    (let ((parent-dir (file-name-directory
                       (directory-file-name default-directory))))
      (when parent-dir
        (my/consult-ripgrep-restart
         (lambda () parent-dir)))))

  (defun my/consult-ripgrep-change-directory ()
    (interactive)
    (my/consult-ripgrep-restart
     (lambda () (nth 2 (consult--directory-prompt "grep" '(4))))))

  (consult-customize
   consult-ripgrep
   :keymap (let ((map (make-sparse-keymap)))
             (define-key map (kbd "M-d") #'my/consult-ripgrep-change-directory)
             (define-key map (kbd "M-l") #'my/consult-ripgrep-up-directory)
             map))
  (defun my/consult-ripgrep ()
    (interactive)
    (my/consult-command 'consult-ripgrep default-directory))

  :bind (
         ("C-x b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("M-g i" . consult-imenu)
         ("M-s r" . my/consult-ripgrep)
         ("M-s g" . consult-git-grep)
         ("M-s f" . consult-find)
         ("M-s l" . consult-line)
         ("C-y" . consult-yank-from-kill-ring)
         ("M-y" . consult-yank-pop)
         ;;("M-s z" . prot/counsel-fzf-rg-files)
         ;;:map ivy-minibuffer-map
         ;;("C-r" . counsel-minibuffer-history)
         ;;("s-y" . ivy-next-line)        ; Avoid 2× `counsel-yank-pop'
         ;;("C-SPC" . ivy-restrict-to-matches)))
         ;;("C-s" . my/consult-line-forward)
         ;;("C-r" . my/consult-line-backward)
         ("M-g g" . consult-goto-line)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch         ;; Minibuffer history
         :map minibuffer-local-map
         ("C-y" . yank)
         ("M-y" . yank-pop)
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)))


(provide 'vitja-vertico)
;;; vitja-vertico.el ends here
