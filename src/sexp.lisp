(in-package #:xml-protocol)

;;; xmls-shaped sexp:
;;;   (name attrs child*)
;;;   name = string qname
;;;   attrs = ((name . value)*)
;;;   child = string | sexp

(defun node-from-sexp (sexp)
  (cond
    ((stringp sexp) (make-instance 'xml-text :data sexp))
    ((and (consp sexp) (stringp (first sexp)))
     (destructuring-bind (name &optional attrs &rest children) sexp
       (apply #'make-xml-element name (or attrs nil)
              (mapcar #'node-from-sexp children))))
    (t (error 'xml-parse-error
              :message (format nil "not an xml sexp: ~S" sexp)))))

(defun node-to-sexp (node)
  (cond
    ((xml-document-p node)
     (let ((root (document-root-element node)))
       (if root (node-to-sexp root) nil)))
    ((xml-element-p node)
     (list* (xml-qname node)
            (mapcar (lambda (a)
                      (cons (xml-qname a) (xml-attribute-value a)))
                    (xml-element-attributes node))
            (loop for c in (xml-element-children node)
                  unless (or (xml-comment-p c) (xml-pi-p c) (xml-doctype-p c))
                    collect (cond
                              ((xml-text-p c) (xml-text-data c))
                              ((xml-cdata-p c) (xml-cdata-data c))
                              ((xml-element-p c) (node-to-sexp c))
                              ((stringp c) c)))))
    ((xml-text-p node) (xml-text-data node))
    ((xml-cdata-p node) (xml-cdata-data node))
    (t nil)))
