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

;;; Spread is a puzzle game in which the player (or players) attempt to
;;; capture the largest ammount of 'area' in the fewest turns.  This area
;;; is represented by a numeric grid.  For example:

;;;     5333|
;;;     44541
;;;     12224
;;;     51434
;;;     _3545

;;; The '_' represents the player's position, while the '|' represents the
;;; opponent's.  On each player's turn, he or she will choose a number.  They
;;; will then capture all area adjacent to their currently controlled area,
;;; which contains the chosen number.  Player's alternate turns in this fasion
;;; until all area is captured.  Using the previous board, suppose the player
;;; chose '5'.  The board would then look like this:

;;;     5333|
;;;     44541
;;;     12224
;;;     _1434
;;;     _3545

;;; If the opponent chose '3', the board would become:

;;;     5||||
;;;     44541
;;;     12224
;;;     _1434
;;;     _3545

;;; Note that the opponent's choice of '3' captures all '3's adjacent to
;;; his or her area, even if that adjacency is the result of area captured
;;; during the course of the current 'move'.  The amount of area controlled
;;; by each player as well as the number of turns taken is displayed at the
;;; bottom of the screen.  In the above example, the following would be
;;; displayed:

;;;     Turns: 1, Score: 2, Opponent: 4

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
(defvar spread-start-position nil)
(defvar spread-ai-text-properties '(:background "red" :foreground "red"))
(defvar spread-player-text-properties '(:background "steel blue" :foreground "steel blue"))
(defvar spread-use-styled-text t)

(defun spread-number-to-char (number)
  "Convert NUMBER to a character."
  (+ number 48))

(defun spread-move-up (&optional n)
  "Move directly up from the point N lines."
  (let ((n (or n 1)))
   (when (> (- (point) (1+ spread-columns)) 0)
     (goto-char (- (point) (+ n (* n spread-columns)))))))

(defun spread-move-down (&optional n)
  "Move directly down from the point N lines."
  (let ((n (or n 1)))
   (goto-char (+ (point) (+ n (* n spread-columns))))))

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
  (when (<= value spread-values)
    (spread-spread value spread-player-char)
    (spread-ai-move)
    (setq spread-turns (1+ spread-turns))
    (spread-update-turns)
    (spread-update-scores)))

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

    (let ((best nil)
          (best-count -1)
          (temp-count -1)
          (n '()))
     (if (not spread-ai-last-move)
         (setq spread-ai-last-move (random spread-values))

       (--dotimes spread-values (setq n (cons (1+ it) n)))
       (setq n (--remove (eq it spread-ai-last-move) n))

       (--each n (when (> (setq temp-count (count-matches
                                            (format "\\(%s%s\\)\\|\\(%s%s\\)"
                                                    (number-to-string it)
                                                    (char-to-string spread-ai-char)
                                                    (char-to-string spread-ai-char)
                                                    (number-to-string it))
                                            (point-min) (point-max)))
                          best-count)
                   (setq best it)
                   (setq best-count temp-count)))
       (setq spread-ai-last-move best)))
    (spread-spread spread-ai-last-move spread-ai-char)))

(defun spread-generate-field (rows columns values &optional start-position)
  "Draw the field with size ROWS and COLUMNS, and VALUES different values.
START-POSITION may be one of three values.  If it is nil or 'symmmetric,
the player and ai will start in random symmetrical positions.  If it is
'opposite, the players will be positioned diagonally across from each
other.  If START-POSITION has the value 'random, starting positions are
completely randomized."
  (let ((buffer-read-only nil))
   (erase-buffer)
   (--dotimes rows
     (--dotimes columns
       (insert (number-to-string (1+ (random values)))))
     (newline))
   (beginning-of-buffer)
   (cond ((eq start-position 'opposite)
          (end-of-line)
          (backward-delete-char 1)
          (spread-insert-styled-char spread-ai-char)
          (end-of-buffer)
          (forward-line -1)
          (delete-char 1)
          (spread-insert-styled-char spread-player-char)
          (backward-char))
         ((or (eq start-position 'symmetric)
              (eq start-position nil))
          (let ((random-x (+ 2 (random (- columns 2))))
                (random-y (+ 2 (random (- rows 2)))))
            (end-of-line)
            (backward-char random-x)
            (spread-move-down random-y)
            (delete-char 1)
            (spread-insert-styled-char spread-ai-char)
            (end-of-buffer)
            (forward-line -1)
            (forward-char random-x)
            (spread-move-up random-y)
            (backward-delete-char 1)
            (spread-insert-styled-char spread-player-char)))
         ((eq start-position 'random)
          (forward-line (1+ (random (- rows 2))))
          (forward-char (1+ (random (- columns 2))))
          (delete-char 1)
          (spread-insert-styled-char spread-player-char)
          (beginning-of-buffer)
          (forward-line (1+ (random (- rows 2))))
          (forward-char (1+ (random (- columns 2))))
          (delete-char 1)
          (spread-insert-styled-char spread-ai-char)))))

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
                             spread-start-position)
      (end-of-buffer)
      (newline)
      (spread-update-turns)
      (spread-update-scores))))

(defun spread (&optional rows columns values start-position no-styled-text)
  "Play a game with size determined by ROWS and COLUMNS.
The game will have VALUES different values.  START-POSITION may be one of
three values.  If it is nil or 'symmmetric, the player and ai will start
in random symmetrical positions.  If it is 'opposite, the players will be
positioned diagonally across from each other.  If START-POSITION has the
value 'random, starting positions are completely randomized.  If NO-STYLED-TEXT
is non-nil, the 'owned' region for the player and AI willf not be colored."
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
   (setq spread-start-position start-position)
   (spread-generate-field rows columns values start-position)
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
  (--dotimes 9
    (define-key spread-mode-map (number-to-string (1+ it))
      (spread-with-key (1+ it)))))

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
