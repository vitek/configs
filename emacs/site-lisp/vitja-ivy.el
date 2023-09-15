;;; vitja-ivy.el --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ivy
  :defer t
  :delight
  :config

  (defun my/ivy-regex (pattern)
    (if (and (> (length pattern) 0)
             (= (aref pattern 0) ?~))
        (ivy--regex-fuzzy (substring pattern 1))
      (ivy--regex pattern)))

  (ivy-mode 1)

  (setq enable-recursive-minibuffers t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-re-builders-alist
        '(;;(counsel-find-file . ivy--regex-plus)
          (counsel-rg . my/ivy-regex)
          (counsel-git-grep . my/ivy-regex)
          (swiper-isearch . my/ivy-regex)
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

(use-package swiper
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)
         :map swiper-map
         ("M-s" . swiper-isearch-toggle)))

(use-package isearch
  :bind
  (:map isearch-mode-map
        ("M-s" . swiper-isearch-toggle)))

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
         ("M-g i" . counsel-imenu)
         ("M-s r" . counsel-rg)
         ("M-s g" . counsel-git-grep)
         ("M-s l" . counsel-find-library)
         ("M-s f" . find-grep-dired)
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

(use-package flyspell-correct-ivy
  :after flyspell-correct
  :config
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(provide 'vitja-ivy)
;;; vitja-ivy.el ends here
