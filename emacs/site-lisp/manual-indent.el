;;; manual-indent.el --- Manual indent minor mode

;; Copyright (C) 2017 Victor Makarov

;; Author: Victor Makarov <vitja.makarov@gmail.com>
;; URL: https://github.com/vitek/configs/tree/master/emacs/site-lisp/
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

;; Minor mode for manual indentation in case there is no original major mode for
;; your language or it's broken.  Example usage:
;;
;; (require 'manual-indent)
;; (add-hook 'lua-mode-hook
;;           (lambda ()
;;             (manual-indent-mode 1)
;;             (electric-indent-mode 0)))

;; Keywords: tools

;;; Code:

(make-variable-buffer-local
 (defvar manual-indent-level 4))

(defun manual-indent-calculate-spaces ()
  (save-excursion
    (-
     (skip-syntax-backward " " (line-beginning-position)))))

(defun manual-indent-delete-trailing-whitespaces ()
  (save-excursion
    (let* ((start (current-column)))
        (skip-syntax-backward " ")
          (delete-char (- start (current-column))))))

(defun manual-indent-calculate-indent ()
  (save-excursion
    (progn
      (beginning-of-line)
      (skip-syntax-forward " ")
      (current-column))))

(defun manual-indent-tab (&optional count)
  (interactive "p")
  (let* ((count (or count 1))
         (odd (% (current-column) manual-indent-level)))
    (insert (make-string (- (* count manual-indent-level) odd) ? ))))

(defun manual-indent-enter (&optional arg)
  (interactive "p")
  (let* ((count (or arg 1))
         (indentation (manual-indent-calculate-indent)))
    (skip-syntax-forward " ")
    (manual-indent-delete-trailing-whitespaces)
    (newline count)
    (insert (make-string indentation ? ))))

(defun manual-indent-backspace (&optional arg)
  (interactive "p")
  (let ((count (or arg 1))
        (spaces (manual-indent-calculate-spaces)))
    (delete-char
     (if (and (> spaces 0) (= 0 (% spaces manual-indent-level)))
         (- manual-indent-level) -1))
    (if (> count 1) (manual-indent-backspace (- count 1)))))

(define-minor-mode manual-indent-mode
  "Toggle manual indent mode"
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " MI"
  ;; The minor mode bindings.
  :keymap
  '(([?\t] . manual-indent-tab)
    ([backspace] . manual-indent-backspace)
    ([?\r] . manual-indent-enter)))

(provide 'manual-indent)
;;; manual-indent.el ends here
