(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)

(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game over."))
  (when (monsters-dead)
    (princ "Congratulations! You have vanguised all of your foes.")))

(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless monsters-dead)
      (show-monsters)
      (player-attack)))
  (fresh-line)
  (map ('list
        (lambda (m)
          (or (monster-dead m) (monster-attach m)))
        *monsters*)
       (game-loop)))

(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
 (fresh-line)
 (princ "You are a valiant knight with a health of ")
 (princ *player-health*)
 (princ ", an agility of ")
 (princ *player-agility*)
 (princ ", and a strength of ")
 (princ *player-strength*))
