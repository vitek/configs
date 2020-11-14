;;; mc-move.el --- Mcedit alike word movements for Emacs -*- lexical-binding: t; -*-

;; Keywords: tools

;; Copyright (C) 2009-2020 Victor Makarov

;; Author: Victor Makarov <vitja.makarov@gmail.com>
;; URL: https://github.com/vitek/configs/tree/master/emacs/site-lisp/mc-move.el
;; Version: 0.2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements mc-move-mode which overrides `forward-word`,
;; `backward-word`, `kill-word`, `backward-kill-word` functions with its
;; own logic which were originally inspired by mcedit.
;;
;; It works this way:
;; - when moving cursor is set at the begining of the word
;; - word is an alpha-numeric sequence samecase or capitalized
;; - symbols are threated as different kind of word
;; - only whitespaces are skipped
;;
;; This all allow to easily navigate over the code.  You can stop at parts of
;; under_score, CamelCase or minus-hyphen style words.  And delete that part
;; backward and forward.
;;
;; In order enable it add the following commands to your Emacs config:
;;
;;  (require 'mc-move)
;;  (global-mc-move-mode)
;;
;;  Or with use-package:
;;
;;  (use-package mc-move
;;    :config
;;    (global-mc-move-mode))

;;; Code:

(defvar mc-move-word "[:alnum:]")
(defvar mc-move-delims " \t\n")
(defvar mc-move-spaces "\t ")
(defvar mc-move-specials "^[:alnum:] \t\n")
(defvar mc-move-upper-words "[:upper:][:digit:]")
(defvar mc-move-lower-words "[:lower:][:digit:]")

(defvar mc-move--installed nil
  "Internal flag indicating that mc-move is installed.")

(defconst mc-move--orig-forward-word (symbol-function 'forward-word))
(defconst mc-move--orig-backward-word (symbol-function 'backward-word))
(defconst mc-move--orig-kill-word (symbol-function 'kill-word))
(defconst mc-move--orig-backward-kill-word
  (symbol-function 'backward-kill-word))

;;;###autoload
(define-minor-mode mc-move-mode
  "Toggle mc-move mode."
  :init-value nil
  :lighter ""
  (mc-move-install))

;;;###autoload
(define-globalized-minor-mode global-mc-move-mode mc-move-mode mc-move-mode-on)

(defun mc-move-mode-on ()
  "Enable mc-move-mode."
  (mc-move-mode 1))

(defun mc-move-by-word (count)
  "Do the move COUNT times.  Negative COUNT for backward direction."
  (if (> count 0)
      (dotimes (_ count)
        (progn
          (cond
           ((> (skip-chars-forward mc-move-delims) 0) 1)
           ((> (+ (skip-chars-forward mc-move-upper-words)
                  (skip-chars-forward mc-move-lower-words))
               0)
            (skip-chars-forward mc-move-spaces))
           ((skip-chars-forward mc-move-specials)
            (skip-chars-forward mc-move-delims))))))
  (dotimes (_ (- count))
    (progn
      (skip-chars-backward mc-move-spaces)
      (cond
       ((< (skip-chars-backward "\n") 0) 1)
       ((< (+ (skip-chars-backward mc-move-lower-words)
              (skip-chars-backward mc-move-upper-words))
           0)
        2)
       ((skip-chars-backward mc-move-specials) 0)))))

(defun mc-move-forward-word (&optional arg)
  "Custom version of `forward-word`.
With argument ARG, do this that many times."
  (interactive "p")
  (if mc-move-mode
      (if arg
          (mc-move-by-word arg)
        (mc-move-by-word 1))
    (funcall mc-move--orig-forward-word arg)))

(defun mc-move-backward-word (&optional arg)
  "Custom version of `backword-word`.
With argument ARG, do this that many times."
  (interactive "p")
  (if mc-move-mode
      (if arg
          (mc-move-by-word (- arg))
        (mc-move-by-word -1))
    (funcall mc-move--orig-backward-word arg)))

(defun mc-move-kill-word (&optional arg)
  "Custom version of `kill-word`.
With argument ARG, do this that many times."
  (interactive "p")
  (if mc-move-mode
      (kill-region (point) (progn (mc-move-forward-word arg) (point)))
    (funcall mc-move--orig-kill-word arg)))

(defun mc-move-backward-kill-word (&optional arg)
  "Custom version of `backward-kill-word`.
With argument ARG, do this that many times."
  (interactive "p")
  (if mc-move-mode
      (mc-move-kill-word (- arg))
    (funcall mc-move--orig-backward-kill-word arg)))

(defun mc-move-install ()
  "Override Emacs 'move by word' functions with `mc-move` versions.

Overrides: 'forward-word, 'backward-word, 'kill-word, 'backward-kill-word"
  (interactive)
  (when (not mc-move--installed)
    (fset 'forward-word 'mc-move-forward-word)
    (fset 'backward-word 'mc-move-backward-word)
    (fset 'kill-word 'mc-move-kill-word)
    (fset 'backward-kill-word 'mc-move-backward-kill-word)
    (setq mc-move--installed t)))

(defun mc-move-uninstall ()
  "Go back to Emacs original functions overridden by 'mc-move-install."
  (interactive)
  (when mc-move--installed
    (fset 'forward-word mc-move--orig-forward-word)
    (fset 'backward-word mc-move--orig-backward-word)
    (fset 'kill-word mc-move--orig-kill-word)
    (fset 'backward-kill-word mc-move--orig-backward-kill-word)
    (setq mc-move--installed nil)))

(defun mc-move-unload-function ()
  "Unload the mc-move library."
  (mc-move-uninstall))

(provide 'mc-move)
;;; mc-move.el ends here
