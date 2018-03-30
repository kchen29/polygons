(defsystem "engine"
  :components
  ((:file "display")
   (:file "matrix")
   (:file "edges" :depends-on ("matrix"))
   (:file "draw" :depends-on ("display" "edges"))
   (:file "parser" :depends-on ("draw"))
   (:file "main" :depends-on ("parser"))))
