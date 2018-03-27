;;;; Primitive functions for edges.

(defun make-edges ()
  "Makes edges."
  (make-matrix))

(defun add-edge (edges x0 y0 z0 x1 y1 z1)
  "Adds a line from point (x0 y0 z0) to (x1 y1 z1) to EDGES.
   Adjusts the array."
  (add-point edges x0 y0 z0)
  (add-point edges x1 y1 z1))

(defun add-point (edges x y z)
  "Adds a point (x y z) to EDGES at INDEX."
  (let ((index (1- (m-last-col edges))))
    (when (= (m-last-col edges) (m-cols edges))
      (adjust-matrix edges 4 (setf (m-cols edges)
                                   (* 2 (m-cols edges)))))
    (incf (m-last-col edges))
    (setf (mref edges 0 index) x
          (mref edges 1 index) y
          (mref edges 2 index) z
          (mref edges 3 index) 1)))
