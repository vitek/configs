;;; early-init.el -*- lexical-binding: t; -*-
;; Based on https://github.com/hlissner/doom-emacs/blob/develop/early-init.el

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; ;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; ;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; ;; we must prevent Emacs from doing it early!
;;(setq package-enable-at-startup nil)
;;(fset #'package--ensure-init-file #'ignore)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Prevent unwanted runtime builds in gccemacs (native-comp); packages are
;; compiled ahead-of-time when they are installed and site files are compiled
;; when gccemacs is installed.
(setq comp-deferred-compilation nil)

;; enable emacs native compilation
(when (version<= "28" emacs-version)
  (setq package-native-compile t))

(setenv "LSP_USE_PLISTS" "true")

;; ;; Workaround for startup interface flickering
;; (add-to-list 'default-frame-alist
;;               '(background-color . "#3F3F3F"))
;; (add-to-list 'default-frame-alist
;;               '(foreground-color . "#DCDCCC"))
;; (add-to-list 'default-frame-alist
;;              '(background-color . "#000000"))
;; (add-to-list 'default-frame-alist
;;              '(foreground-color . "#FFFFFF"))

;;; early-init.el ends here
