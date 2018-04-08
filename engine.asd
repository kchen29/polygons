(defsystem "engine"
  :components
  ((:file "utils")
   (:file "display")
   (:file "matrix")
   (:file "edges" :depends-on ("matrix" "utils"))
   (:file "draw" :depends-on ("display" "edges"))
   (:file "parser" :depends-on ("draw"))
   (:file "main" :depends-on ("parser"))))
