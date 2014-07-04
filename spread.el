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

(defun spread-number-to-char (number)
  "Convert NUMBER to a character."
  (+ number 48))

(defun spread-spread-point (value)
  "For the current point, spread to all neightbors containing VALUE."
  (let ((buffer-read-only nil))
   (save-excursion
     (when (or (eq (following-char) ?_)
               (eq (following-char) value))
       (delete-char 1)
       (insert ?_)
       (backward-char)

       (save-excursion (forward-char)
                       (unless (eq (following-char) ?_)
                         (spread-spread-point value)))

       (save-excursion (artist-previous-line 1)
                       (unless (eq (following-char) ?_)
                         (spread-spread-point value)))

       (save-excursion (artist-previous-line -1)
                       (unless (eq (following-char) ?_)
                         (spread-spread-point value)))

       (unless (bobp)
        (save-excursion (backward-char)
                        (unless (eq (following-char) ?_)
                          (spread-spread-point value))))))))

(defun spread-spread (value)
  "Spread to all adjacent cells containing VALUE."
  (interactive)
  (save-excursion
   (beginning-of-buffer)
   (while (re-search-forward "_" nil t)
     (backward-char)
     (spread-spread-point (spread-number-to-char value))
     (forward-char))
   (setq spread-area ())))

(defun spread-generate-field (rows columns values)
  "Draw the field with size ROWS and COLUMNS, and VALUES different values."
  (let ((buffer-read-only nil))
   (erase-buffer)
   (--dotimes rows
     (--dotimes columns
       (insert (number-to-string (1+ (random values)))))
     (newline))
   (forward-line -1)
   (delete-char 1)
   (insert ?_)
   (backward-char)))

(defun spread (&optional rows columns values)
  "Play a game with size determined by ROWS and COLUMNS.
The game will have VALUES different values."
  (interactive)
  (switch-to-buffer "*spread*")
  (spread-mode)
  (buffer-disable-undo (current-buffer))
  (setq rows (if (not rows) spread-rows rows))
  (setq columns (if (not columns) spread-columns columns))
  (setq values (if (not values) spread-values values))
  (spread-generate-field rows columns values)
  (setq buffer-read-only t))

(defvar spread-mode-map nil)
(unless spread-mode-map
  (setq spread-mode-map (make-keymap))
  (suppress-keymap spread-mode-map t)
  (define-key spread-mode-map "1" (lambda () (interactive) (spread-spread 1)))
  (define-key spread-mode-map "2" (lambda () (interactive) (spread-spread 2)))
  (define-key spread-mode-map "3" (lambda () (interactive) (spread-spread 3)))
  (define-key spread-mode-map "4" (lambda () (interactive) (spread-spread 4)))
  (define-key spread-mode-map "5" (lambda () (interactive) (spread-spread 5))))

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
