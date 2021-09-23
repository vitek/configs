;;; mc-move.el --- Mcedit alike word movements for Emacs -*- lexical-binding: t; -*-

;; Keywords: tools

;; Copyright (C) 2009-2021 Victor Makarov

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

(defgroup mc-move nil
  "Smart movements by word parts."
  :group 'convenience)

;;;###autoload
(define-minor-mode mc-move-mode
  "Toggle mc-move mode."
  :group mc-move
  :init-value nil
  :lighter "")

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
    (forward-word arg)))

(defun mc-move-backward-word (&optional arg)
  "Custom version of `backword-word`.
With argument ARG, do this that many times."
  (interactive "p")
  (if mc-move-mode
      (if arg
          (mc-move-by-word (- arg))
        (mc-move-by-word -1))
    (backward-word arg)))

(defun mc-move-kill-word (&optional arg)
  "Custom version of `kill-word`.
With argument ARG, do this that many times."
  (interactive "p")
  (if mc-move-mode
      (kill-region (point) (progn (mc-move-forward-word arg) (point)))
    (kill-word arg)))

(defun mc-move-backward-kill-word (&optional arg)
  "Custom version of `backward-kill-word`.
With argument ARG, do this that many times."
  (interactive "p")
  (if mc-move-mode
      (mc-move-kill-word (- arg))
    (backward-kill-word arg)))

(defun mc-move--mark-word (&optional arg allow-extend)
  "Set mark ARG words away from point.
The place mark goes is the same place \\[forward-word] would
move to with the same argument.
Interactively, if this command is repeated
or (in Transient Mark mode) if the mark is active,
it marks the next ARG words after the ones already marked."
  (cond ((and allow-extend
              (or (and (eq last-command this-command) (mark t))
                  (region-active-p)))
         (setq arg (if arg (prefix-numeric-value arg)
                     (if (< (mark) (point)) -1 1)))
         (set-mark
          (save-excursion
            (goto-char (mark))
            (mc-move-forward-word arg)
            (point))))
        (t
         (push-mark
          (save-excursion
            (mc-move-forward-word (prefix-numeric-value arg))
            (point))
          nil t))))


(defun mc-move-mark-word (&optional arg allow-extend)
  (interactive "P\np")
  (if mc-move-mode
      (mc-move--mark-word arg allow-extend)
      (mark-word arg allow-extend)))

(provide 'mc-move)
;;; mc-move.el ends here
