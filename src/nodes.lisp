(in-package #:xml-protocol)

(defclass xml-node () ())

(defclass xml-document (xml-node)
  ((version :initarg :version :accessor xml-document-version :initform "1.0")
   (encoding :initarg :encoding :accessor xml-document-encoding :initform "UTF-8")
   (standalone :initarg :standalone :accessor xml-document-standalone :initform nil)
   (children :initarg :children :accessor xml-document-children :initform nil)))

(defun xml-document-p (object)
  (typep object 'xml-document))

(defclass xml-element (xml-node)
  ((local-name :initarg :local-name :accessor xml-element-local-name :initform "")
   (prefix :initarg :prefix :accessor xml-element-prefix :initform nil)
   (namespace-uri :initarg :namespace-uri :accessor xml-element-namespace-uri :initform nil)
   (attributes :initarg :attributes :accessor xml-element-attributes :initform nil)
   (namespace-decls :initarg :namespace-decls :accessor xml-element-namespace-decls :initform nil)
   (children :initarg :children :accessor xml-element-children :initform nil)))

(defun xml-element-p (object)
  (typep object 'xml-element))

(defclass xml-attribute ()
  ((local-name :initarg :local-name :accessor xml-attribute-local-name :initform "")
   (prefix :initarg :prefix :accessor xml-attribute-prefix :initform nil)
   (namespace-uri :initarg :namespace-uri :accessor xml-attribute-namespace-uri :initform nil)
   (value :initarg :value :accessor xml-attribute-value :initform "")
   (specified-p :initarg :specified-p :accessor xml-attribute-specified-p :initform t)))

(defun xml-attribute-p (object)
  (typep object 'xml-attribute))

(defclass xml-namespace ()
  ((prefix :initarg :prefix :accessor xml-namespace-prefix :initform nil)
   (uri :initarg :uri :accessor xml-namespace-uri :initform "")))

(defun xml-namespace-p (object)
  (typep object 'xml-namespace))

(defclass xml-text (xml-node)
  ((data :initarg :data :accessor xml-text-data :initform "")))

(defun xml-text-p (object)
  (typep object 'xml-text))

(defclass xml-cdata (xml-node)
  ((data :initarg :data :accessor xml-cdata-data :initform "")))

(defun xml-cdata-p (object)
  (typep object 'xml-cdata))

(defclass xml-comment (xml-node)
  ((data :initarg :data :accessor xml-comment-data :initform "")))

(defun xml-comment-p (object)
  (typep object 'xml-comment))

(defclass xml-pi (xml-node)
  ((target :initarg :target :accessor xml-pi-target :initform "")
   (data :initarg :data :accessor xml-pi-data :initform "")))

(defun xml-pi-p (object)
  (typep object 'xml-pi))

(defclass xml-doctype (xml-node)
  ((name :initarg :name :accessor xml-doctype-name :initform "")
   (public-id :initarg :public-id :accessor xml-doctype-public-id :initform nil)
   (system-id :initarg :system-id :accessor xml-doctype-system-id :initform nil)))

(defun xml-doctype-p (object)
  (typep object 'xml-doctype))

(defun split-qname (name)
  (let ((pos (and (stringp name) (position #\: name))))
    (if pos
        (values (subseq name 0 pos) (subseq name (1+ pos)))
        (values nil (or name "")))))

(defun xml-qname (node)
  (etypecase node
    (xml-element
     (let ((prefix (xml-element-prefix node))
           (local (xml-element-local-name node)))
       (if (and prefix (plusp (length prefix)))
           (concatenate 'string prefix ":" local)
           local)))
    (xml-attribute
     (let ((prefix (xml-attribute-prefix node))
           (local (xml-attribute-local-name node)))
       (if (and prefix (plusp (length prefix)))
           (concatenate 'string prefix ":" local)
           local)))
    (string node)))

(defun xml-local-name (node)
  (etypecase node
    (xml-element (xml-element-local-name node))
    (xml-attribute (xml-attribute-local-name node))
    (string (nth-value 1 (split-qname node)))))

(defun %attr-matches (attr name)
  (or (string-equal (xml-qname attr) name)
      (string-equal (xml-attribute-local-name attr) name)))

(defun xml-attr (element name &optional default)
  (check-type element xml-element)
  (let ((found (find-if (lambda (a) (%attr-matches a name))
                        (xml-element-attributes element))))
    (if found (xml-attribute-value found) default)))

(defun xml-named-p (element name)
  (and (xml-element-p element)
       (string= (xml-element-local-name element) name)))

(defun xml-child (element name)
  (find-if (lambda (c) (xml-named-p c name))
           (xml-element-children element)))

(defun xml-children-named (element name)
  (loop for c in (xml-element-children element)
        when (xml-named-p c name)
          collect c))

(defun xml-element-text (element)
  (with-output-to-string (s)
    (dolist (c (xml-element-children element))
      (cond
        ((xml-text-p c) (write-string (xml-text-data c) s))
        ((xml-cdata-p c) (write-string (xml-cdata-data c) s))
        ((stringp c) (write-string c s))))))

(defun document-root-element (document)
  (find-if #'xml-element-p (xml-document-children document)))

(defun %coerce-attr (pair)
  (cond
    ((xml-attribute-p pair) pair)
    ((and (consp pair) (or (stringp (car pair)) (symbolp (car pair))))
     (multiple-value-bind (prefix local)
         (split-qname (if (stringp (car pair))
                          (car pair)
                          (string-downcase (symbol-name (car pair)))))
       (make-instance 'xml-attribute
                      :prefix prefix
                      :local-name local
                      :value (princ-to-string (cdr pair)))))
    (t (error 'xml-encode-error
              :message (format nil "not an attribute: ~S" pair)))))

(defun %ns-from-attrs (attrs)
  (loop for a in attrs
        for qname = (xml-qname a)
        for value = (xml-attribute-value a)
        when (string= qname "xmlns")
          collect (make-instance 'xml-namespace :prefix nil :uri value)
        when (and (> (length qname) 6) (string= qname "xmlns:" :end1 6))
          collect (make-instance 'xml-namespace
                                 :prefix (subseq qname 6)
                                 :uri value)))

(defun %lookup-ns (decls prefix)
  (let ((found (find prefix decls
                     :key #'xml-namespace-prefix
                     :test (lambda (a b)
                             (cond
                               ((null a) (null b))
                               ((null b) nil)
                               (t (string= a b)))))))
    (and found (xml-namespace-uri found))))

(defun make-xml-element (name &optional attrs &rest children)
  "NAME is a qname string. ATTRS is an alist or list of xml-attribute."
  (multiple-value-bind (prefix local) (split-qname name)
    (let* ((attr-objs (mapcar #'%coerce-attr (remove nil attrs)))
           (decls (%ns-from-attrs attr-objs))
           (uri (or (%lookup-ns decls prefix)
                    (and (null prefix) (%lookup-ns decls nil))))
           (kids (loop for c in children
                       unless (null c)
                         collect (cond
                                   ((xml-node-p c) c)
                                   ((stringp c) (make-instance 'xml-text :data c))
                                   (t c)))))
      (make-instance 'xml-element
                     :prefix prefix
                     :local-name local
                     :namespace-uri uri
                     :attributes attr-objs
                     :namespace-decls decls
                     :children kids))))

(defun xml-node-p (object)
  (typep object 'xml-node))

(defun make-xml-document (&key (version "1.0") (encoding "UTF-8") standalone children)
  (make-instance 'xml-document
                 :version version
                 :encoding encoding
                 :standalone standalone
                 :children children))
