(in-package #:xml-protocol)

(defvar *xml-backend* nil
  "Current XML backend object.")

(defgeneric backend-encode (backend value &key stream pretty declaration)
  (:documentation "Encode VALUE (xml-document or xml-element) to a string, or STREAM."))

(defgeneric backend-decode (backend source &key)
  (:documentation "Decode SOURCE (string, octets, stream, pathname) to xml-document."))

(defun encode (value &key stream (pretty t) (declaration t))
  "Encode VALUE via *XML-BACKEND*. Returns a string unless STREAM is given."
  (unless *xml-backend*
    (error 'xml-encode-error :message "*xml-backend* is unbound — load xml-backend-native"))
  (backend-encode *xml-backend* value :stream stream :pretty pretty :declaration declaration))

(defun decode (source &key)
  "Decode SOURCE via *XML-BACKEND* → xml-document."
  (unless *xml-backend*
    (error 'xml-parse-error :message "*xml-backend* is unbound — load xml-backend-native"))
  (backend-decode *xml-backend* source))

(defun encode-to-octets (value &key (pretty t) (declaration t))
  (babel:string-to-octets (encode value :pretty pretty :declaration declaration)
                          :encoding :utf-8))

(defun decode-octets (octets &key)
  (decode octets))

(defun install-http-xml-hooks ()
  "If http-protocol is loaded, bind application/xml serdes. No-op when absent."
  (let ((pkg (find-package :http-protocol)))
    (unless pkg
      (return-from install-http-xml-hooks nil))
    (let ((ser (find-symbol "*DATA-SERIALIZERS*" pkg))
          (des (find-symbol "*DATA-DESERIALIZERS*" pkg)))
      (when (and ser (boundp ser))
        (setf (symbol-value ser)
              (acons :xml #'encode
                     (remove :xml (symbol-value ser) :key #'car))))
      (when (and des (boundp des))
        (setf (symbol-value des)
              (acons :xml #'decode
                     (remove :xml (symbol-value des) :key #'car)))))
    t))
