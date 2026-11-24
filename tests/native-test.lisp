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

(deftest character-references
  "xml-protocol#4: a character reference that is not an XML 1.0 Char
   (past #x10FFFF, a bignum, &#0;, a surrogate, #xFFFE) or has a bad digit
   signals XML-WELL-FORMED-ERROR, as lxml raises XMLSyntaxError; it used to
   be a TYPE-ERROR from CODE-CHAR."
  (xml-backend-native:use-native-backend)
  (ok (string= (format nil "a~c~c~cb" (code-char #xE9) (code-char #x10FFFF) #\Tab)
               (apply #'concatenate 'string
                      (mapcar #'xml-text-data
                              (remove-if-not #'xml-text-p
                                             (xml-element-children
                                              (document-root-element
                                               (decode "<r>a&#xE9;&#x10FFFF;&#9;b</r>")))))))
      "legal references, hex and decimal")
  (ok (string= "A" (xml-attr (document-root-element (decode "<r v=\"&#x0041;\"/>")) "v"))
      "in an attribute, leading zeros")
  (dolist (text '("<r>&#x110000;</r>" "<r>&#99999999999999999999;</r>" "<r>&#0;</r>"
                  "<r>&#xD800;</r>" "<r>&#xFFFE;</r>" "<r>&#1;</r>" "<r>&#;</r>"
                  "<r>&#x;</r>" "<r>&#12a;</r>" "<r>&#-5;</r>" "<r>&#x+1F;</r>"
                  "<r>&#65</r>" "<r v=\"&#x110000;\"/>"))
    (ok (signals (decode text) 'xml-well-formed-error) text)))

(deftest damaged-documents-signal-xml-error
  "Any damage to a well-formed document signals an XML-ERROR or parses:
   never another condition. 3000 documents, each with up to 4 characters
   replaced by XML punctuation, digits or a non-ASCII character (fixed
   seed)."
  (xml-backend-native:use-native-backend)
  (let* ((base (concatenate 'string
                            "<?xml version=\"1.0\"?><!--c--><w:d xmlns:w=\"urn:w\" a='1'>"
                            "<w:p q=\"&amp;&#x41;\"><w:t>x &lt; y &#233;</w:t><![CDATA[<z>]]>"
                            "</w:p><?pi d?><e/></w:d>"))
         (alphabet "<>&#;x0129aAF\"'=/?!-[]: ")
         (seed 42)
         (other '()))
    (flet ((random* (n)
             ;; A fixed LCG, so every Lisp damages the same way.
             (setf seed (mod (+ (* seed 1103515245) 12345) #x80000000))
             (mod (ash seed -8) n)))
      (dotimes (n 3000)
        (let ((text (copy-seq base)))
          (dotimes (k (1+ (random* 4)))
            (setf (char text (random* (length text)))
                  (if (zerop (random* 10))
                      (code-char (+ #x80 (random* #x3000)))
                      (char alphabet (random* (length alphabet))))))
          (handler-case (decode text)
            (xml-error () nil)
            (error (e) (push (list text (type-of e)) other))))))
    (ok (null other) (format nil "~d not XML-ERROR, first: ~s" (length other) (first other)))))

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
