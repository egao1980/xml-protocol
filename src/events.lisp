(in-package #:xml-protocol)

(defclass xml-event-writer ()
  ((backend :initarg :backend :reader event-writer-backend)
   (stream :initarg :stream :reader event-writer-stream)))

(defgeneric backend-make-event-writer (backend stream &key pretty declaration)
  (:documentation "Create a streaming XML event writer for STREAM."))

(defgeneric write-event (writer event &optional value)
  (:documentation "Write EVENT (keyword or xml-element subtree) to WRITER."))

(defmethod backend-make-event-writer ((backend xml-backend) stream &key pretty declaration)
  (declare (ignore stream pretty declaration))
  (error 'xml-encode-error
         :message (format nil "event writer not implemented for ~A"
                          (class-name (class-of backend)))))

(defmethod write-event ((writer xml-event-writer) event &optional value)
  (declare (ignore event value))
  (error 'xml-encode-error :message "write-event not implemented"))

(defun make-event-writer (stream &key pretty (declaration t))
  (unless *xml-backend*
    (error 'xml-encode-error :message "*xml-backend* is unbound — load xml-backend-native"))
  (backend-make-event-writer *xml-backend* stream
                             :pretty pretty :declaration declaration))

(defmacro with-event-writer ((var stream &key pretty (declaration t)) &body body)
  `(let ((,var (make-event-writer ,stream :pretty ,pretty :declaration ,declaration)))
     ,@body))
