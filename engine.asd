(defsystem "engine"
  :components
  ((:file "display")
   (:file "matrix")
   (:file "edges" :depends-on ("matrix"))
   (:file "draw" :depends-on ("edges"))
   (:file "parser" :depends-on ("display" "draw"))
   (:file "main" :depends-on ("parser"))))
