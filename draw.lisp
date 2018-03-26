;;;; Draw to screen. Add things to edges.

(defun plot (x y screen color)
  "Plots (x, y) on the 2D array SCREEN with COLOR.
   Rounds x and y. Checks bounds. COLOR is not copied."
  (setf x (round x) y (round y))
  (when (and (< -1 x (array-dimension screen 0)) (< -1 y (array-dimension screen 1)))
    (setf (aref screen x y) color)))

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
     (plot ,plot-1 ,plot-2 screen color)
     (when (> d 0)
       (incf y)
       (incf d 2B))))

(defun draw-line (x0 y0 x1 y1 screen color)
  "Draws a line from (x0, y0) to (x1, y1) on SCREEN using COLOR."
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

(defun draw-lines (edges screen color)
  "Draws the lines from EDGES onto SCREEN with COLOR."
  (loop while (car edges)
        for p0 = (pop edges)
        for p1 = (pop edges)
        do (draw-line (first p0) (second p0) (first p1) (second p1) screen color)))

(defun add-parametric (edges step x-function y-function &optional (z 0))
  "Given X-FUNCTION and Y-FUNCTION, which take one input and outputs the x and y
   coordinates respectively, add a parametric where s runs from 0 to 1 at STEP interval
   and add the connecting lines to EDGES.
   Optionally takes a z value, defaulted to 0, where all the points are shifted by z."
  (flet ((get-x (s) (funcall x-function s))
         (get-y (s) (funcall y-function s)))
    (do* ((s step (+ s step))
          (prev-x (get-x 0) x)
          (prev-y (get-y 0) y)
          (x (get-x s) (get-x s))
          (y (get-y s) (get-y s)))
         ((>= s (1+ step)))
      (add-edge edges prev-x prev-y z x y z))))

(defun add-circle (edges step x y z radius)
  "Add a circle to EDGES with center (x y) and RADIUS with STEP interval. Circle shifted by Z."
  (add-parametric edges step
                  (lambda (s) (+ x (* radius (cos (* 2 pi s)))))
                  (lambda (s) (+ y (* radius (sin (* 2 pi s)))))
                  z))

(defun evaluate-polynomial (x &rest coefficients)
  "Evaluates a polynomial in X with COEFFICIENTS. Starts from the least power (x^0) and
   increases with each coefficient."
  (loop for coeff in coefficients
        for product = 1 then (* x product)
        sum (* coeff product)))

(defun add-hermite (edges step x0 y0 x1 y1 dx0 dy0 dx1 dy1)
  "Add a hermite curve to EDGES with points (x0 y0) and (x1 y1) and the rates wrt. time of
   the corresponding coordinates (dx0 dy0) and (dx1 dy1), with STEP interval."
  (add-parametric edges step
                  (get-hermite-cubic x0 x1 dx0 dx1)
                  (get-hermite-cubic y0 y1 dy0 dy1)))

(defun get-hermite-cubic (x0 x1 dx0 dx1)
  "Returns the function, given the coordinate (x0 x1) and rates of changes (dx0 dx1),
   taking in a time and returning the output on a hermite cubic curve."
  (lambda (s) (evaluate-polynomial s
                                   x0 dx0
                                   (- (* 3 x1) (* 3 x0) (* 2 dx0) dx1)
                                   (+ (* 2 x0) (* -2 x1) dx0 dx1))))

(defun add-bezier (edges step x0 y0 x1 y1 x2 y2 x3 y3)
  "Add a bezier curve to EDGES with endpoints (x0 y0) and (x3 y3).
   (x1 y1) and (x2 y2) are control points. Drawn with STEP interval."
  (add-parametric edges step
                  (get-bezier-cubic x0 x1 x2 x3)
                  (get-bezier-cubic y0 y1 y2 y3)))

(defun get-bezier-cubic (x0 x1 x2 x3)
  "Returns the function, given the x coordinates, taking in a time and returning the output
   on a bezier cubic curve."
  (lambda (s) (evaluate-polynomial s
                                   x0 (* 3 (- x1 x0))
                                   (- (* 3 (+ x0 x2)) (* 6 x1))
                                   (+ (* 3 (- x1 x2)) (- x3 x0)))))

(defun add-square (edges x y z width height)
  "Adds a square to EDGES where the front left point ix (x y z).
   Draws it parallel to the xy plane."
  (let ((right (+ x width))
        (down (- y height)))
    (add-edge edges x y z right y z)
    (add-edge edges x y z x down z)
    (add-edge edges right y z right down z)
    (add-edge edges x down z right down z)))

(defun add-box (edges x y z width height depth)
  "Adds a box to EDGES where the front left upper point is (x y z).
   WIDTH is x, HEIGHT y, and DEPTH z."
  (let ((right (+ x width))
        (down (- y height))
        (back (- z depth)))
    (add-square edges x y z width height)
    (add-square edges x y back width height)
    (add-edge edges x y z x y back)
    (add-edge edges right y z right y back)
    (add-edge edges x down z x down back)
    (add-edge edges right down z right down back)))

(defun generate-sphere (step x y z r)
  "Generates a sphere with center (x y z), radius R, points drawn STEP times."
  (let (points)
    (loop for phi below step
          for s = (* 2 pi (/ phi step))
          do (loop for theta below step
                   for v = (* pi (/ theta step))
                   do (push (list (+ x (* r (cos s)))
                                  (+ y (* r (sin s) (cos v)))
                                  (+ z (* r (sin s) (sin v)))
                                  1)
                            points)))
    points))
             
(defun add-sphere (edges step x y z r)
  "Adds a sphere to EDGES."
  (loop for point in (generate-sphere step x y z r)
        for p1 = (first point)
        for p2 = (second point)
        for p3 = (third point)
        do (add-edge edges p1 p2 p3 (+ 3 p1) (+ 3 p2) (+ 3 p3))))

(defun generate-torus (step x y z r1 r2)
  "Generates a torus with center (x y z), cross-section circle radius R1,
   rotated around with radius R2. Points drawn STEP times."
  (let (points)
    (loop for phi below step
          for s = (* 2 pi (/ phi step))
          do (loop for theta below step
                   for v = (* 2 pi (/ theta step))
                   do (push (list (+ x (* (cos s) (+ r2 (* r1 (cos v)))))
                                  (- y (* r1 (sin v)))
                                  (- z (* (sin s) (+ r2 (* r1 (cos v)))))
                                  1)
                            points)))
    points))

(defun add-torus (edges step x y z r1 r2)
  "Adds a torus to EDGES."
  (loop for point in (generate-torus step x y z r1 r2)
        for p1 = (first point)
        for p2 = (second point)
        for p3 = (third point)
        do (add-edge edges p1 p2 p3 (+ 3 p1) (+ 3 p2) (+ 3 p3))))
