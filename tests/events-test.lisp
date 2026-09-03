(in-package #:xml-protocol/tests)

(defun %events (source)
  (let ((parser (serdes-protocol:make-event-parser source :format :xml))
        (acc '()))
    (loop
      (multiple-value-bind (ev val) (serdes-protocol:parse-next-event parser)
        (unless ev (return))
        (push (list ev val) acc)))
    (nreverse acc)))

(deftest event-parser-simple
  (xml-backend-native:use-native-backend)
  (let ((evs (%events "<a><b>x</b></a>")))
    (ok (eq :start-document (first (first evs))))
    (ok (eq :start-element (first (second evs))))
    (ok (string= "a" (xml-local-name (second (second evs)))))
    (ok (eq :end-document (first (car (last evs)))))))

(deftest parse-next-element
  (xml-backend-native:use-native-backend)
  (let* ((parser (serdes-protocol:make-event-parser
                  "<!--c--><root><n>z</n></root>" :format :xml))
         (el (serdes-protocol:parse-next-element parser)))
    (ok (xml-element-p el))
    (ok (string= "root" (xml-local-name el)))
    (ok (string= "z" (xml-element-text (xml-child el "n"))))))
