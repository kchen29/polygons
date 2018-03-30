(defun main (filename)
  "Sets up then parses FILENAME. See parser.lisp."
  (let* ((edges (make-edges))
         (polygons (make-edges))
         (transform (make-transform-matrix)))
    (parse-file filename edges polygons transform)))
