;;;; Draw to screen.

(defun plot (x y color)
  "Plots (x, y) on *SCREEN* with COLOR.
   Rounds x and y. Checks bounds. COLOR is not copied."
  (setf x (round x) y (round y))
  (when (and (< -1 x *screen-side*) (< -1 y *screen-side*))
    (setf (aref *screen* x y) color)))

(defmacro draw-line-base (x0 y0 x1 y1 plot-1 plot-2)
  "Base code for octant 1. Other octants can be gotten from transformations."
  `(do* ((x ,x0 (1+ x))
         (y ,y0)
         (A (- ,y1 ,y0))
         (B (- ,x0 ,x1))
         (2A (* 2 A))
         (2B (* 2 B))
         (d (+ 2A B) (+ d 2A)))
        ((> x ,x1))
     (plot ,plot-1 ,plot-2 color)
     (when (> d 0)
       (incf y)
       (incf d 2B))))

(defun draw-line (x0 y0 x1 y1 color)
  "Draws a line from (x0, y0) to (x1, y1) on *SCREEN* using COLOR."
  (when (minusp (- x1 x0))
    (rotatef x0 x1)
    (rotatef y0 y1))
  (let ((xdif (- x1 x0))
        (ydif (- y1 y0)))
    (if (>= ydif 0)
        (if (minusp (- ydif xdif))
            (draw-line-base x0 y0 x1 y1 x y)
            (draw-line-base y0 x0 y1 x1 y x))
        (if (minusp (+ ydif xdif))
            (draw-line-base y0 x0 (- y0 ydif) x1 y (- (* 2 y0) x))
            (draw-line-base x0 y0 x1 (- y0 ydif) x (- (* 2 y0) y))))))

(defun draw-lines (edges color)
  "Draws the lines from EDGES to *SCREEN* with COLOR."
  (do ((index 0 (+ 2 index)))
      ((>= index (m-last-col edges)))
    (draw-line (mref edges 0 index)
               (mref edges 1 index)
               (mref edges 0 (1+ index))
               (mref edges 1 (1+ index))
               color)))

;;;3d shapes
(defun draw-polygons (polygons color)
  "Draws the polygons from POLYGONS to *SCREEN* with COLOR."
  (flet ((draw-polygon (x0 y0 x1 y1 x2 y2)
           (draw-line x0 y0 x1 y1 color)
           (draw-line x0 y0 x2 y2 color)
           (draw-line x1 y1 x2 y2 color)))
    (do ((index 0 (+ 3 index)))
        ((>= index (m-last-col polygons)))
      (when (forward-facing polygons index)
        (draw-polygon (mref polygons 0 index)
                      (mref polygons 1 index)
                      (mref polygons 0 (1+ index))
                      (mref polygons 1 (1+ index))
                      (mref polygons 0 (+ 2 index))
                      (mref polygons 1 (+ 2 index)))))))

;;closure, for efficiency
(let ((temp1 (make-array 3))
      (temp2 (make-array 3)))
  (defun forward-facing (polygons index)
    "Returns true if the surface in POLYGONS starting 
     at INDEX is forward-facing."
    (dotimes (x 3)
      (setf (svref temp1 x) (- (mref polygons x index)
                               (mref polygons x (1+ index)))
            (svref temp2 x) (- (mref polygons x index)
                               (mref polygons x (+ 2 index)))))
    (plusp (- (* (svref temp1 0)
                 (svref temp2 1))
              (* (svref temp2 0)
                 (svref temp1 1))))))
