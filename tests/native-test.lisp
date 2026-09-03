(in-package #:xml-protocol/tests)

(deftest-parametrize roundtrip
    ((xml)
     ("<root/>")
     ("<root><n a=\"1\">hi</n></root>")
     ("<root xmlns=\"urn:x\"><n>z</n></root>")
     ("<p:r xmlns:p=\"urn:p\"><p:n/></p:r>")
     ("<root><!--c--><n/></root>")
     ("<root><?pi data?><n/></root>")
     ("<root><![CDATA[a<b>]]></root>")
     ("<root>&amp;&lt;&gt;</root>"))
  (xml-backend-native:use-native-backend)
  (let* ((doc (decode xml))
         (out (encode doc :pretty nil))
         (doc2 (decode out)))
    (ok (string= (xml-local-name (document-root-element doc))
                 (xml-local-name (document-root-element doc2))))))

(deftest namespaces
  (xml-backend-native:use-native-backend)
  (let* ((doc (decode "<p:r xmlns:p=\"urn:p\" xmlns:q=\"urn:q\" q:a=\"1\"><p:n/></p:r>"))
         (root (document-root-element doc)))
    (ok (string= "r" (xml-local-name root)))
    (ok (string= "urn:p" (xml-element-namespace-uri root)))
    (ok (string= "1" (xml-attr root "a")))
    (ok (string= "urn:q" (xml-attribute-namespace-uri
                          (find "a" (xml-element-attributes root)
                                :key #'xml-attribute-local-name
                                :test #'string=))))))

(deftest comments-and-pi-kept
  (xml-backend-native:use-native-backend)
  (let* ((doc (decode "<!--hi--><?p x?><root/>"))
         (kids (xml-document-children doc)))
    (ok (xml-comment-p (first kids)))
    (ok (string= "hi" (xml-comment-data (first kids))))
    (ok (xml-pi-p (second kids)))
    (ok (xml-element-p (third kids)))))

(deftest xsi-nil-opaque
  (xml-backend-native:use-native-backend)
  (let* ((doc (decode "<n xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:nil=\"true\"/>"))
         (el (document-root-element doc)))
    (ok (string-equal "true" (xml-attr el "nil")))
    (ok (string-equal "true" (xml-attr el "xsi:nil")))))

(deftest mismatched-tags
  (xml-backend-native:use-native-backend)
  (ok (signals (decode "<a></b>") 'xml-well-formed-error)))

(deftest unknown-entity
  (xml-backend-native:use-native-backend)
  (ok (signals (decode "<a>&foo;</a>") 'xml-entity-error)))

(deftest external-dtd
  (xml-backend-native:use-native-backend)
  (ok (signals (decode "<!DOCTYPE r SYSTEM \"x.dtd\"><r/>") 'xml-entity-error)))

(deftest depth-limit
  (xml-backend-native:use-native-backend)
  (let ((parser (serdes-protocol:make-event-parser "<a><b/></a>" :format :xml :max-depth 1)))
    (ok (signals (loop (unless (serdes-protocol:parse-next-event parser) (return)))
                 'xml-limit-error))))

(deftest string-limit
  (xml-backend-native:use-native-backend)
  (let ((parser (serdes-protocol:make-event-parser "<a>abcd</a>" :format :xml
                                                               :max-string-length 2)))
    (ok (signals (loop (unless (serdes-protocol:parse-next-event parser) (return)))
                 'xml-limit-error))))
