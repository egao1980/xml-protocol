(in-package #:xml-protocol/tests)

(deftest make-element-and-attr
  (let ((el (make-xml-element "xs:schema"
                              `(("xmlns:xs" . "http://www.w3.org/2001/XMLSchema")
                                ("version" . "1.0"))
                              (make-xml-element "xs:element" '(("name" . "n"))))))
    (ok (string= "schema" (xml-local-name el)))
    (ok (string= "xs:schema" (xml-qname el)))
    (ok (string= "1.0" (xml-attr el "version")))
    (ok (xml-named-p (xml-child el "element") "element"))
    (ok (= 1 (length (xml-children-named el "element"))))))

(deftest sexp-roundtrip
  (let* ((sexp '("root" (("a" . "1")) "hi" ("n" nil)))
         (node (node-from-sexp sexp))
         (back (node-to-sexp node)))
    (ok (string= "root" (first back)))
    (ok (equal '("a" . "1") (assoc "a" (second back) :test #'string=)))
    (ok (find "hi" (cddr back) :test #'equal))))
