;;;; Interacting with screens. Primarily interfacing to the outside.

(defun write-ppm (filename dimensions screen)
  "Writes a ppm, assuming P3 and max color value of 255.
   Writes to FILENAME, with DIMENSIONS and 2D array of colors SCREEN."
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (screen-to-destination stream dimensions screen)))

(defun screen-to-destination (destination dimensions screen)
  "Turns SCREEN into a string, which is then passed on to DESTINATION via format"
  (format destination
          "P3 ~a ~a 255 ~{~%~{~{~a ~a ~a ~}~}~}"
          (first dimensions) (second dimensions) (screen-to-list screen)))

(defun screen-to-list (screen)
  "Turns a 2D array SCREEN into a list.
   Places (0, 0) on the lower left corner of the list."
  (loop for y from (1- (array-dimension screen 1)) downto 0
        collect (loop for x below (array-dimension screen 0)
                      collect (aref screen x y))))

(defun save (filename dimensions screen)
  "Saves SCREEN to filename.
   Attempts conversion using imagemagick's convert if filename is not a ppm."
  (if (equal (pathname-type (pathname filename)) "ppm")
      (write-ppm filename dimensions screen)
      (run-program "convert" (list "-" filename)
                   :input (make-string-input-stream (screen-to-destination nil dimensions screen))
                   :wait nil :search t)))

(defun clear-screen (screen)
  "Clears SCREEN. Sets all the pixels to be black."
  (dotimes (x (array-dimension screen 0))
    (dotimes (y (array-dimension screen 1))
      (setf (aref screen x y) '(0 0 0)))))

(defun display (dimensions screen &key (wait nil))
  "Displays the image with SCREEN.
   If WAIT is t, then will wait until display ends
   Uses imagemagick's display to display an image."
  (run-program "display" (list "-")
               :input (make-string-input-stream (screen-to-destination nil dimensions screen))
               :wait wait :search t))
