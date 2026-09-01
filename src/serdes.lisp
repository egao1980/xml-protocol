(in-package #:xml-protocol)

(defclass xml-serdes-backend (serdes-protocol:serdes-backend) ()
  (:documentation "serdes backend that delegates to *XML-BACKEND*."))

(defclass xml-character-input-stream (serdes-protocol:serdes-character-input-stream) ())
(defclass xml-character-output-stream (serdes-protocol:serdes-character-output-stream) ())

(defun make-xml-serdes-backend ()
  (make-instance 'xml-serdes-backend))

(defmethod serdes-protocol:backend-encode ((backend xml-serdes-backend) value &key stream)
  (declare (ignore backend))
  (encode value :stream stream))

(defmethod serdes-protocol:backend-decode ((backend xml-serdes-backend) source &key)
  (declare (ignore backend))
  (decode source))

(defmethod serdes-protocol:backend-make-input-stream ((backend xml-serdes-backend) underlying
                                                      &key (element-type 'character))
  (unless (subtypep element-type 'character)
    (error 'xml-error :message (format nil "unsupported element-type ~S" element-type)))
  (make-instance 'xml-character-input-stream
                 :underlying underlying :backend backend))

(defmethod serdes-protocol:backend-make-output-stream ((backend xml-serdes-backend) underlying
                                                       &key (element-type 'character))
  (unless (subtypep element-type 'character)
    (error 'xml-error :message (format nil "unsupported element-type ~S" element-type)))
  (make-instance 'xml-character-output-stream
                 :underlying underlying :backend backend))

(defmethod serdes-protocol:stream-encode-value ((stream xml-character-output-stream) value &key)
  (encode value :stream (serdes-protocol:underlying-stream stream) :pretty nil)
  value)

(defmethod serdes-protocol:stream-decode-value ((stream xml-character-input-stream) &key)
  (let ((in (serdes-protocol:underlying-stream stream)))
    (if (null (peek-char nil in nil nil))
        :eof
        (decode in))))

(defun use-xml-serdes-backend ()
  (let ((backend (make-xml-serdes-backend)))
    (serdes-protocol:register-format :xml backend)
    (setf serdes-protocol:*serdes-format* :xml
          serdes-protocol:*serdes-backend* backend)
    backend))

(defun install-serdes-xml-hooks ()
  (unless *xml-backend*
    (return-from install-serdes-xml-hooks nil))
  (use-xml-serdes-backend)
  t)
