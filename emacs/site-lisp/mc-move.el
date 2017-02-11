;;; mc-move.el --- Mcedit alike word movements for Emacs

;; Keywords: tools

;; Copyright (C) 2009-2017 Victor Makarov

;; Author: Victor Makarov <vitja.makarov@gmail.com>
;; URL: https://github.com/vitek/configs/tree/master/emacs/site-lisp/mc-move.el
;; Version: 0.1

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

;; This package implements its own versions of `forward-word`, `backward-word`,
;; `kill-word`, `backward-kill-word` which were originally inspired by mcedit
;; behaviour.  It treats undersocre and capital letter as the begining of new
;; word.  Which is very useful when working with variable and function names.
;; To enable it just require:
;;
;;  (require 'mc-move)

;;; Code:

(defvar mc-move-word "[:alnum:]")
(defvar mc-move-delims " \t\n")
(defvar mc-move-spaces "\t ")
(defvar mc-move-specials "^[:alnum:] \t\n")
(defvar mc-move-upper-words "[:upper:][:digit:]")
(defvar mc-move-lower-words "[:lower:][:digit:]")

(defun mc-move-by-word (count)
  "Do the move COUNT times.  Negative COUNT for backward direction."
  (if (> count 0)
      (dotimes (i count)
        (progn
          (cond
           ((> (skip-chars-forward mc-move-delims) 0) 1)
           ((> (+ (skip-chars-forward mc-move-upper-words)
                  (skip-chars-forward mc-move-lower-words)) 0)
            (skip-chars-forward mc-move-spaces))
           ((skip-chars-forward mc-move-specials)
            (skip-chars-forward mc-move-delims))))))
  (dotimes (i (- count))
    (progn
      (skip-chars-backward mc-move-spaces)
      (cond
       ((< (skip-chars-backward "\n") 0) 1)
       ((< (+ (skip-chars-backward mc-move-lower-words)
              (skip-chars-backward mc-move-upper-words)) 0) 2)
       ((skip-chars-backward mc-move-specials) 0)))))

(defun mc-move-forward-word (&optional arg)
  "Custom version of `forward-word`.
With argument ARG, do this that many times."
  (interactive "p")
  (if arg
      (mc-move-by-word arg)
    (mc-move-by-word 1)))

(defun mc-move-backward-word (&optional arg)
  "Custom version of `backword-word`.
With argument ARG, do this that many times."
  (interactive "p")
  (if arg
      (mc-move-by-word (- arg))
    (mc-move-by-word -1)))

(defun mc-move-kill-word (&optional arg)
  "Custom version of `kill-word`.
With argument ARG, do this that many times."
  (interactive "p")
  (kill-region (point) (progn (mc-move-forward-word arg) (point))))

(defun mc-move-backward-kill-word (&optional arg)
  "Custom version of `backward-kill-word`.
With argument ARG, do this that many times."
  (interactive "p")
  (mc-move-kill-word (- arg)))

(defalias 'forward-word 'mc-move-forward-word)
(defalias 'backward-word 'mc-move-backward-word)
(defalias 'kill-word 'mc-move-kill-word)
(defalias 'backward-kill-word 'mc-move-backward-kill-word)

(provide 'mc-move)
;;; mc-move.el ends here
