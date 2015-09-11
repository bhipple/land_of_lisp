;; ============================================================================
;;                                  Globals
;; ============================================================================
; Association list with the world locations => descriptions
(defparameter *nodes* '((living-room (you are in the living-room.
                            a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                            there is a well in front of you.))
                        (attic (you are in the attic.
                            there is a giant welding torch in the corner.))))

(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

(defparameter *objects* '(whiskey bucket frog chain))
(defparameter *nerdobjects* '(bike dueji goyani pandi))

(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

(defparameter *nerd-object-locations* '((bike living-room)
                                        (dueji living-room)
                                        (goyani living-room)
                                        (pandi garden)))

(defparameter *location* 'living-room)

(defparameter *allowed-commands* '(look walk pickup inventory))

;; ============================================================================
;;                           Objects and Movement
;; ============================================================================
; Helper function to describe a vertex in our graph
(defun describe-location (location nodes)
   (cadr (assoc location nodes)))

; Helper function with quasi-quoting to describe an edge
; The initial ` says that we're going into data mode, but
; it allows for us to temporarily switch back to code mode
; with ,()
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

; Note that the #' is not required in Scheme, only Common Lisp.
; This is because Common Lisp keeps one namespace for functions
; and a separate one for variables.
(defun describe-paths (location edges)
 (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun objects-at (loc objs obj-locs)
 (labels ((at-loc-p (obj)
           (eq (cadr (assoc obj obj-locs)) loc)))
  (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc)
 (labels ((describe-obj (obj)
            `(you see a ,obj on the floor.)))
  (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;; ============================================================================
;;                               API Functions
;; ============================================================================
(defun pickup (object)
 (cond ((member object
                (objects-at *location* *objects* *object-locations*))
        (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))

(defun inventory ()
 (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun look()
 (append (describe-location *location* *nodes*)
         (describe-paths *location* *edges*)
         (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
 (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
 (if next
    (progn (setf *location* (car next))
           (look))
    '(you cannot go that way.))))

;; ============================================================================
;;                                 Game REPL
;; ============================================================================
(defun game-read ()
 (let ((cmd (read-from-string
                    (concatenate 'string "(" (read-line) ")"))))
 (flet ((quote-it (x)
            (list 'quote x)))
    (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

; Prevents hacks against the read-eval procedure
(defun game-eval (sexp)
 (if (member (car sexp) *allowed-commands*)
     (eval sexp)
     '(i do not know that command.)))

; Handle converting from all uppercase to natural English case.
(defun tweak-text (lst caps lit)
 (when lst
 (let ((item (car lst))
       (rest (cdr lst)))
 (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
       ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
       ((eq item #\") (tweak-text rest caps (not lit)))
        (lit (cons item (tweak-text rest nil lit)))
       ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
       (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
 (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (prin1-to-string lst))
                                    'list)
                            t
                            nil)
                'string))
(fresh-line))

(defun game-repl ()
 (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
        (game-print (game-eval cmd))
        (game-repl))))

(defun start ()
 (print (cons "Available commmands: " (prin1-to-string *allowed-commands*)))
 (fresh-line)
 (game-repl))
