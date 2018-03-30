(defun main (filename)
  "Sets up then parses FILENAME. See parser.lisp."
  (let* ((edges (make-matrix))
         (polygons (make-matrix))
         (transform (make-transform-matrix)))
    (parse-file filename edges polygons transform)))
