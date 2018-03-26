;;;; Primitive functions for edges.

(defun make-edges ()
  "Makes edges."
  (make-matrix 4 0))

(defun add-edge (edges x0 y0 z0 x1 y1 z1)
  "Adds a line from point (x0 y0 z0) to (x1 y1 z1) to EDGES.
   Adjusts the array."
  (let ((dim (array-dimension edges 1)))
    (adjust-array edges (list 4 (+ 2 dim)))
    (add-point edges x0 y0 z0 dim)
    (add-point edges x1 y1 z1 (1+ dim))))

(defun add-point (edges x y &optional (z 0) (index (1- (array-dimension edges 1))))
  "Adds a point (x y z) to EDGES at INDEX. Use add-edge."
  (setf (aref edges 0 index) x
        (aref edges 1 index) y
        (aref edges 2 index) z
        (aref edges 3 index) 1))

(defun clear-edges (edges)
  "Clears EDGES."
  (clear-matrix edges))
