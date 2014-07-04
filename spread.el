;;; spread.el --- A simple puzzle game for GNU Emacs

;; Copyright (c) 2014 Adam Schwalm
;; Author: Adam Schwalm <adamschwalm@gmail.com>
;; Version: 0.1

;; This program is free software: you can redistribute it and/or modify
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

;;; Code:

(require 'dash)

(defvar spread-rows 40)
(defvar spread-columns 40)
(defvar spread-values 5)
(defvar spread-prev-value nil)
(defvar spread-area ())

(defun spread-number-to-char (number)
  "Convert NUMBER to a character."
  (+ number 48))

(defun spread-spread-point (value)
  "For the current point, spread to all neightbors containing VALUE."
  (save-excursion
    (unless (member (point) spread-area)
      (setq spread-area (cons (point) spread-area))
      (when (eq (following-char) spread-prev-value)
        (delete-char 1)
        (insert value)
        (backward-char)

        (save-excursion (forward-char)
                        (spread-spread-point value))

        (save-excursion (forward-line -1)
                        (spread-spread-point value))

        (save-excursion (forward-line)
                        (spread-spread-point value))

        (save-excursion (backward-char)
                        (spread-spread-point value))))))

(defun spread-spread (value)
  "Spread to all adjacent cells containing VALUE."
  (interactive)
  (spread-spread-point (spread-number-to-char value))
  (setq spread-prev-value (spread-number-to-char value))
  (setq spread-area ()))

(defun spread-generate-field (rows columns values)
  "Draw the field with size ROWS and COLUMNS, and VALUES different values."
  (erase-buffer)
  (--dotimes rows
    (--dotimes columns
      (insert (number-to-string (random values))))
    (newline))
  (forward-line -1)
  (setq spread-prev-value (following-char)))

(defun spread (&optional rows columns values)
  "Play a game with size determined by ROWS and COLUMNS.
The game will have VALUES different values."
  (interactive)
  (spread-mode)
  (setq rows (if (not rows) spread-rows rows))
  (setq columns (if (not columns) spread-columns columns))
  (setq values (if (not values) spread-values values))
  (spread-generate-field rows columns values))

(defvar spread-mode-map nil)
(unless spread-mode-map
  (setq spread-mode-map (make-keymap))
  (suppress-keymap spread-mode-map t))

(define-derived-mode spread-mode nil "spread"
  "A simple mode for spread
\\{spread-mode-map}"
  (kill-all-local-variables)
  (use-local-map spread-mode-map)
  (setq major-mode 'spread-mode)
  (setq mode-name "spread")
  (setf show-trailing-whitespace nil))

(provide 'spread)
;;; spread.el ends here
