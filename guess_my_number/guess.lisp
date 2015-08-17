; defparameter lets us define a global variable
; The *'s around the name are called "earmuffs", and
; are a common idiomatic way of denoting global variables
(defparameter *small* 1)
(defparameter *big* 100)

; ash is the binary shift operator. -1 as
; the argument tells it to do a right shift, which
; halves the number
(defun guess-my-number ()
    (ash (+ *small* *big*) -1))

(defun smaller()
    (setf *big* (1- (guess-my-number)))
    (guess-my-number))

(defun bigger()
    (setf *small* (1+ (guess-my-number)))
    (guess-my-number))

(defun start-over ()
    (defparamater *small* 1)
    (defparameter *big* 100)
    (guess-my-number))
