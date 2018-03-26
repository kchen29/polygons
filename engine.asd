(defsystem "engine"
  :components
  ((:file "display")
   (:file "matrix")
   (:file "edges")
   (:file "draw" :depends-on ("edges"))
   (:file "parser" :depends-on ("display" "matrix" "draw"))
   (:file "main" :depends-on ("parser"))))
