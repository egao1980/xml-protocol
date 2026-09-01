(defsystem "xml-protocol"
  :version "0.1.0"
  :description "CLOS XML Infoset + pull events + writer; implements serdes-protocol :xml"
  :author "egao1980"
  :license "MIT"
  :depends-on ("babel" "serdes-protocol")
  :properties (:cl-repo (:ci (:with ("xml-backend-native"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "nodes")
               (:file "events")
               (:file "protocol")
               (:file "serdes")
               (:file "sexp"))
  :in-order-to ((test-op (test-op "xml-protocol/tests"))))

(defsystem "xml-protocol/tests"
  :depends-on ("xml-protocol" "xml-backend-native" "serdes-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "nodes-test")
               (:file "events-test")
               (:file "writer-test")
               (:file "serdes-test")
               (:file "native-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
