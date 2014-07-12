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
(require 'artist)

(defvar spread-rows 40)
(defvar spread-columns 40)
(defvar spread-values 5)
(defvar spread-turns 0)
(defvar spread-player-char ?_)
(defvar spread-ai-char ?|)
(defvar spread-ai-last-move nil)
(defvar spread-player-score 1)
(defvar spread-ai-score 1)

(defun spread-number-to-char (number)
  "Convert NUMBER to a character."
  (+ number 48))

(defun spread-spread-point (value char)
  "For the current point, spread to all neightbors containing VALUE."
  (let ((buffer-read-only nil))
   (save-excursion
     (when (or (eq (following-char) char)
               (eq (following-char) value))
       (delete-char 1)
       (insert char)
       (backward-char)

       (unless (eolp)
        (save-excursion (forward-char)
                        (unless (eq (following-char) char)
                          (spread-spread-point value char))))

       (save-excursion (artist-previous-line 1)
                       (unless (eq (following-char) char)
                         (spread-spread-point value char)))

       (save-excursion (artist-previous-line -1)
                       (unless (eq (following-char) char)
                         (spread-spread-point value char)))

       (unless (bolp)
        (save-excursion (backward-char)
                        (unless (eq (following-char) char)
                          (spread-spread-point value char))))))))

(defun spread-spread (value char)
  "Spread to all adjacent cells containing VALUE from cells containing CHAR."
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward (char-to-string char) nil t)
      (backward-char)
      (spread-spread-point (spread-number-to-char value) char)
      (forward-char))
    (setq spread-area ())))

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
     (with-no-warnings (end-of-buffer))
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
  (let ((move (1+ (random spread-values))))
    (if spread-ai-last-move
        (while (eq move spread-ai-last-move)
          (setq move (1+ (random spread-values)))))
    (spread-spread move spread-ai-char)
    (setq spread-ai-last-move move)))

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
         (insert spread-ai-char)
         (end-of-buffer)
         (forward-line -1)
         (delete-char 1)
         (insert spread-player-char)
         (backward-char))
     (forward-line (1+ (random (- rows 2))))
     (forward-char (1+ (random (- columns 2))))
     (delete-char 1)
     (insert spread-player-char)
     (beginning-of-buffer)
     (forward-line (1+ (random (- rows 2))))
     (forward-char (1+ (random (- columns 2))))
     (delete-char 1)
     (insert spread-ai-char))))

(defun spread (&optional rows columns values random-start)
  "Play a game with size determined by ROWS and COLUMNS.
The game will have VALUES different values.  If RANDOM-START is not nill,
the player and AI starting positions will be randomized."
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
