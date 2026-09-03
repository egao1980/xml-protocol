(in-package #:xml-protocol)

(define-condition xml-error (error)
  ((message :initarg :message :reader xml-error-message :initform nil)
   (line :initarg :line :reader xml-error-line :initform nil)
   (column :initarg :column :reader xml-error-column :initform nil))
  (:report (lambda (c s)
             (format s "XML error~@[: ~A~]~@[ at ~A:~A~]"
                     (xml-error-message c)
                     (xml-error-line c)
                     (xml-error-column c)))))

(define-condition xml-parse-error (xml-error) ())
(define-condition xml-encode-error (xml-error) ())
(define-condition xml-well-formed-error (xml-parse-error) ())
(define-condition xml-namespace-error (xml-parse-error) ())
(define-condition xml-entity-error (xml-parse-error) ())
(define-condition xml-limit-error (xml-parse-error) ())

(defclass xml-backend () ()
  (:documentation "Base class for xml-protocol backends."))
