;;;; Primitive functions for edges.

(defun make-edges ()
  "Makes edges. Represented as a 2D list for efficiency.
   Initial edges is a cons of nil."
  (cons nil nil))

(defun add-edge (edges x0 y0 z0 x1 y1 z1)
  "Adds a line from point (x0 y0 z0) to (x1 y1 z1) to EDGES."
  (add-point edges x1 y1 z1)
  (add-point edges x0 y0 z0))

(defun add-point (edges x y z)
  "Adds a point (x y z) to EDGES."
  ;;can't change edges directly, caller's edges would stay the same
  ;;edges starts out as a cons of nil
  (when (car edges)
    (setf (cdr edges) (cons (car edges) (cdr edges))))
  (setf (car edges) (list x y z 1)))

(defun clear-edges (edges)
  "Clears EDGES."
  (setf (car edges) nil
        (cdr edges) nil))
