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
(defvar spread-turns 0)
(defvar spread-player-char ?_)
(defvar spread-ai-char ?|)
(defvar spread-ai-last-move nil)
(defvar spread-player-score 1)
(defvar spread-ai-score 1)
(defvar spread-random-start nil)
(defvar spread-ai-text-properties '(:background "red" :foreground "red"))
(defvar spread-player-text-properties '(:background "steel blue" :foreground "steel blue"))
(defvar spread-use-styled-text t)

(defun spread-number-to-char (number)
  "Convert NUMBER to a character."
  (+ number 48))

(defun spread-move-up ()
  "Move directly up from the point."
  (when (> (- (point) (1+ spread-columns)) 0)
    (goto-char (- (point) (1+ spread-columns)))))

(defun spread-move-down ()
  "Move directly down from the point."
  (goto-char (+ (point) (1+ spread-columns))))

(defun spread-insert-styled-char (char)
  "Insert CHAR with PROPERTIES."
  (let ((buffer-read-only nil))
    (if spread-use-styled-text
     (if (eq char spread-player-char)
         (insert (propertize (char-to-string char)
                             'face spread-player-text-properties))
       (insert (propertize (char-to-string char)
                           'face spread-ai-text-properties)))
     (insert char))))

(defun spread-spread-point (value char)
  "For the current point, spread to all neightbors containing VALUE."
  (let ((buffer-read-only nil))
   (when (or (eq (following-char) char)
             (eq (following-char) value))
     (unless (eq (following-char) char)
       (delete-char 1)
       (spread-insert-styled-char char)
       (backward-char))

     (unless (eolp)
       (save-excursion (forward-char)
                       (unless (eq (following-char) char)
                         (spread-spread-point value char))))

     (save-excursion (spread-move-up)
                     (unless (eq (following-char) char)
                       (spread-spread-point value char)))

     (save-excursion (spread-move-down)
                     (unless (eq (following-char) char)
                       (spread-spread-point value char)))

     (unless (bolp)
       (save-excursion (backward-char)
                       (unless (eq (following-char) char)
                         (spread-spread-point value char)))))))

(defun spread-spread (value char)
  "Spread to all adjacent cells containing VALUE from cells containing CHAR."
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward (char-to-string char) nil t)
      (backward-char)
      (spread-spread-point (spread-number-to-char value) char)
      (forward-char))))

(defun spread-player-move (value)
  "Spread from the player's area to all cells containing VALUE."
  (interactive)
  (spread-spread value spread-player-char)
  (spread-ai-move)
  (setq spread-turns (1+ spread-turns))
  (spread-update-turns)
  (spread-update-scores))

(defun spread-update-turns ()
  "Update the number of turns displayed."
  (save-excursion
   (let ((buffer-read-only nil))
     (end-of-buffer)
     (beginning-of-line)
     (if (not (eolp)) (kill-line))
     (insert (format "    Turns: %d" spread-turns)))))

(defun spread-update-scores ()
  "Update the scores for the player and ai."
  (setq spread-player-score (count-matches (char-to-string spread-player-char)
                                           (point-min) (point-max)))
  (setq spread-ai-score (count-matches (char-to-string spread-ai-char)
                                           (point-min) (point-max)))

  (end-of-buffer)
  (let ((buffer-read-only nil))
   (insert (format ", Score: %d, Opponent: %d"
                   spread-player-score
                   spread-ai-score))))

(defun spread-ai-move ()
  "Choose the move for the enemy AI."
  (if (not spread-ai-last-move)
      (setq spread-ai-last-move (1+ (random spread-values)))

    (let ((n '()))
     (--dotimes spread-values (setq n (cons (1+ it) n)))
     (setq n (--remove (eq it spread-ai-last-move) n))
     (setq spread-ai-last-move (nth (random (length n)) n)))

    (spread-spread spread-ai-last-move spread-ai-char)
    (message (number-to-string spread-ai-last-move))))

(defun spread-generate-field (rows columns values &optional random-start)
  "Draw the field with size ROWS and COLUMNS, and VALUES different values.
If RANDOM-START is not nil, the player and AI starting positions are randomized."
  (let ((buffer-read-only nil))
   (erase-buffer)
   (--dotimes rows
     (--dotimes columns
       (insert (number-to-string (1+ (random values)))))
     (newline))
   (beginning-of-buffer)
   (if (not random-start)
       (progn
         (end-of-line)
         (backward-delete-char 1)
         (spread-insert-styled-char spread-ai-char)
         (end-of-buffer)
         (forward-line -1)
         (delete-char 1)
         (spread-insert-styled-char spread-player-char)
         (backward-char))
     (forward-line (1+ (random (- rows 2))))
     (forward-char (1+ (random (- columns 2))))
     (delete-char 1)
     (spread-insert-styled-char spread-player-char)
     (beginning-of-buffer)
     (forward-line (1+ (random (- rows 2))))
     (forward-char (1+ (random (- columns 2))))
     (delete-char 1)
     (spread-insert-styled-char spread-ai-char))))

(defun spread-reset ()
  "Reset the current game of spread."
  (interactive)
  (if (not (eq major-mode 'spread-mode))
      (message "No spread game to reset.")
    (setq spread-turns 0)
    (setq spread-ai-score 1)
    (setq spread-player-score 1)
    (let ((buffer-read-only nil))
      (spread-generate-field spread-rows
                             spread-columns
                             spread-values
                             spread-random-start)
      (end-of-buffer)
      (newline)
      (spread-update-turns)
      (spread-update-scores))))

(defun spread (&optional rows columns values random-start no-styled-text)
  "Play a game with size determined by ROWS and COLUMNS.
The game will have VALUES different values.  If RANDOM-START is not nill,
the player and AI starting positions will be randomized.  If NO-STYLED-TEXT
is non-nil, the 'owned' region for the player and AI will not be colored."
  (interactive)
  (switch-to-buffer "*spread*")
  (let ((buffer-read-only nil))
   (spread-mode)
   (buffer-disable-undo (current-buffer))
   (setq rows (if (not rows) spread-rows rows))
   (setq columns (if (not columns) spread-columns columns))
   (setq values (if (not values) spread-values values))
   (setq spread-turns 0)
   (setq spread-ai-score 1)
   (setq spread-player-score 1)
   (setq spread-use-styled-text (not no-styled-text))
   (setq spread-random-start random-start)
   (spread-generate-field rows columns values random-start)
   (end-of-buffer)
   (newline)
   (spread-update-turns)
   (spread-update-scores)
   (setq cursor-type nil))
  (setq buffer-read-only t))

(defun spread-with-key (n)
  "Bind the N number key to (spread-spread N)."
  (lexical-let ((n n)) #'(lambda ()
                           (interactive)
                           (spread-player-move n))))

(defvar spread-mode-map nil)
(unless spread-mode-map
  (setq spread-mode-map (make-keymap))
  (suppress-keymap spread-mode-map t)
  (define-key spread-mode-map "q" 'bury-buffer)
  (define-key spread-mode-map "r" 'spread-reset)
  (--dotimes 10
    (define-key spread-mode-map (number-to-string it)
      (spread-with-key it))))

(define-derived-mode spread-mode nil "spread"
  "A mode for playing spread
\\{spread-mode-map}"
  (kill-all-local-variables)
  (use-local-map spread-mode-map)
  (setq major-mode 'spread-mode)
  (setq mode-name "spread")
  (setf show-trailing-whitespace nil))

(provide 'spread)
;;; spread.el ends here
