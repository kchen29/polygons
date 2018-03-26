(defun main (filename)
  "Sets up then parses FILENAME. See parser.lisp."
  (let* ((screen-size 500)
         (dimensions (list screen-size screen-size))
         (screen (make-array dimensions :initial-element '(0 0 0)))
         (edges (make-edges))
         (transform (make-matrix)))
    (parse-file filename edges transform dimensions screen)))
