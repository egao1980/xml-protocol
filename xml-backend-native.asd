(defsystem "xml-backend-native"
  :version "0.1.0"
  :description "xml-protocol backend — first-party well-formed XML 1.0 pull + sink"
  :author "egao1980"
  :license "MIT"
  :depends-on ("xml-protocol" "serdes-protocol" "babel")
  :serial t
  :pathname "src/backend-native"
  :components ((:file "package")
               (:file "parser")
               (:file "writer")
               (:file "backend")))
