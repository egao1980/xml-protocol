(in-package #:xml-protocol/tests)

(deftest serdes-xml-roundtrip
  (xml-backend-native:use-native-backend)
  (let* ((el (make-xml-element "root"
                               '(("xmlns" . "urn:x"))
                               (make-xml-element "n" '(("a" . "1")) "hi")))
         (xml (serdes-protocol:encode el :format :xml))
         (doc (serdes-protocol:decode xml :format :xml))
         (root (document-root-element doc)))
    (ok (search "<root" xml))
    (ok (string= "root" (xml-local-name root)))
    (ok (string= "hi" (xml-element-text (xml-child root "n"))))
    (ok (string= "1" (xml-attr (xml-child root "n") "a")))))

(deftest serdes-stream-value
  (xml-backend-native:use-native-backend)
  (let* ((el (make-xml-element "x" nil "z"))
         (raw (with-output-to-string (o)
                (let ((out (serdes-protocol:make-output-stream o :format :xml)))
                  (serdes-protocol:stream-encode-value out el)))))
    (with-input-from-string (i raw)
      (let* ((in (serdes-protocol:make-input-stream i :format :xml))
             (doc (serdes-protocol:stream-decode-value in)))
        (ok (xml-document-p doc))
        (ok (string= "z" (xml-element-text (document-root-element doc))))
        (ok (eq :eof (serdes-protocol:stream-decode-value in)))))))
