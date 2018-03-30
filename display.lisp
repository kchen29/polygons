;;;; Interacting with screens. Primarily interfacing to the outside.

(defparameter *screen-side* 500)
(defparameter *screen-dimensions* (list *screen-side* *screen-side*))

(defun make-screen ()
  (make-array *screen-dimensions* :initial-element '(0 0 0)))

(defparameter *screen* (make-screen) "A 2D array of colors.")

(defun write-ppm (filename)
  "Writes a ppm, assuming P3 and max color value of 255.
   Writes to FILENAME with *SCREEN*."
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (screen-to-destination stream)))

(defun screen-to-destination (destination)
  "Turns *SCREEN* into a string, which is then passed on to DESTINATION via format."
  (format destination
          "P3 ~a ~a 255 ~{~%~{~{~a ~a ~a ~}~}~}"
          *screen-side* *screen-side* (screen-to-list)))

(defun screen-to-list ()
  "Turns *SCREEN* into a list.
   Places (0, 0) on the lower left corner of the list."
  (loop for y from (1- *screen-side*) downto 0
        collect (loop for x below *screen-side*
                      collect (aref *screen* x y))))

(defun save (filename)
  "Saves *SCREEN* to filename.
   Attempts conversion using imagemagick's convert if filename is not a ppm."
  (if (equal (pathname-type (pathname filename)) "ppm")
      (write-ppm filename)
      (run-program "convert" (list "-" filename)
                   :input (make-string-input-stream (screen-to-destination nil))
                   :wait nil :search t)))

(defun clear-screen ()
  "Clears *SCREEN*. Sets all the pixels to black."
  (dotimes (x *screen-side*)
    (dotimes (y *screen-side*)
      (setf (aref *screen* x y) '(0 0 0)))))

(defun display (&optional (wait nil))
  "Displays the image with *SCREEN*.
   If WAIT is t, then will wait until display ends
   Uses imagemagick's display to display an image."
  (run-program "display" (list "-")
               :input (make-string-input-stream (screen-to-destination nil))
               :wait wait :search t))
