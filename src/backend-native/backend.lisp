(in-package #:xml-backend-native)

(defclass native-backend (xml-backend) ())

(defun make-native-backend ()
  (make-instance 'native-backend))

(defun use-native-backend ()
  (prog1 (setf *xml-backend* (make-native-backend))
    (install-serdes-xml-hooks)))

(defmethod backend-encode ((backend native-backend) value &key stream pretty declaration)
  (if stream
      (write-xml value stream :pretty pretty :declaration declaration)
      (with-output-to-string (s)
        (write-xml value s :pretty pretty :declaration declaration))))

(defmethod backend-decode ((backend native-backend) source &key)
  (let ((parser (make-native-parser source :backend (serdes-protocol:find-backend :xml nil))))
    (unwind-protect
         (parse-document-from parser)
      (funcall (%close parser)))))

(defmethod serdes-protocol:backend-make-event-parser ((backend xml-serdes-backend) source
                                                      &key max-depth max-string-length)
  (make-native-parser source
                      :backend backend
                      :max-depth max-depth
                      :max-string-length max-string-length))

(defmethod serdes-protocol:parse-next-event ((parser native-event-parser))
  (%next-event parser))

(defmethod serdes-protocol:parse-next-element ((parser native-event-parser) &key)
  (loop
    (multiple-value-bind (ev val) (%next-event parser)
      (cond
        ((null ev) (return :eof))
        ((eq ev :start-element)
         (return (%build-element parser val)))
        ((eq ev :end-document) (return :eof))
        ((eq ev :start-document) nil)
        (t nil)))))

(defmethod backend-make-event-writer ((backend native-backend) stream
                                      &key pretty declaration)
  (make-instance 'native-event-writer
                 :backend backend
                 :stream stream
                 :pretty pretty
                 :declaration declaration))

(use-native-backend)
