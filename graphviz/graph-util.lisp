; Provides a library for converting Lisp graphs into graphviz format

; Example graph from the previous exercise
(defparameter *wizard-nodes* '((living-room (you are in the living-room.
                                                 a wizard is snoring loudly on the couch.))
                               (garden (you are in a beautiful garden.
                                            there is a well in front of you.))
                               (attic (you are in the attic.
                                           there is a giant welding torch in the corner.))))

(defparameter *wizard-edges* '((living-room (garden west door)
                                            (attic upstairs ladder))
                               (garden (living-room east door))
                               (attic (living-room downstairs ladder))))

(defparameter *max-label-length* 30)

; Substitute all non-alphanumeric characters with underscores
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defun dot-label (exp)
  (if exp
    (let ((s (write-to-string exp :pretty nil)))
      (if (> (length s) *max-label-length*)
        (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
        s))
    ""))

; This function generates the DOT information for each node
; mapc executes in-place, while mapcar returns a copy
(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
        edges))

(defun uedges->dot (edges)
  (maplist (lambda (lst)
             (mapc (lambda (edge)
                     (unless (assoc (car edge) (cdr lst))
                       (fresh-line)
                       (princ (dot-name (caar lst)))
                       (princ "--")
                       (princ (dot-name (car edge)))
                       (princ "[label=\"")
                       (princ (dot-label (cdr edge)))
                       (princ "\"];")))
                   (cdar lst)))
           edges))

; Put it all together to create the graphiz dotfile
(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges))))

; The thunk is a delayed-execution function that has side-effects
; which dot->png captures. In this case, those side-effects are
; writing to stdout
(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                    fname
                    :direction :output
                    :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname)))

; API
(defun graph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (graph->dot nodes edges))))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges))))
