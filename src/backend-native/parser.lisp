(in-package #:xml-backend-native)

(defparameter *xml-ns* "http://www.w3.org/XML/1998/namespace")
(defparameter *xmlns-ns* "http://www.w3.org/2000/xmlns/")

(defclass native-event-parser (serdes-protocol:serdes-event-parser)
  ((in :initarg :in :accessor %in)
   (close :initarg :close :accessor %close :initform (constantly nil))
   (buf :initform nil :accessor %buf)
   (line :initform 1 :accessor %line)
   (column :initform 0 :accessor %column)
   (depth :initform 0 :accessor %depth)
   (max-depth :initarg :max-depth :accessor %max-depth :initform nil)
   (max-string-length :initarg :max-string-length :accessor %max-string-length :initform nil)
   (stack :initform nil :accessor %stack)
   (ns-stack :initform nil :accessor %ns-stack)
   (phase :initform :prolog :accessor %phase)
   (pending :initform nil :accessor %pending)
   (emitted-start-doc :initform nil :accessor %emitted-start-doc)))

(defun %init-ns (parser)
  (setf (%ns-stack parser) (list `(("xml" . ,*xml-ns*) ("xmlns" . ,*xmlns-ns*)))))

(defun %fail (parser class message)
  (error class :message message :line (%line parser) :column (%column parser)))

(defun %peek (parser)
  (or (car (%buf parser))
      (let ((c (read-char (%in parser) nil :eof)))
        (push c (%buf parser))
        c)))

(defun %next (parser)
  (let ((c (if (%buf parser)
               (pop (%buf parser))
               (read-char (%in parser) nil :eof))))
    (unless (eq c :eof)
      (if (char= c #\Newline)
          (progn (incf (%line parser)) (setf (%column parser) 0))
          (incf (%column parser))))
    c))

(defun %unread-char (parser c)
  (push c (%buf parser)))

(defun %eof-p (parser)
  (eq (%peek parser) :eof))

(defun %whitespace-p (c)
  (and (characterp c)
       (member c '(#\Space #\Tab #\Newline #\Return))))

(defun %skip-ws (parser)
  (loop while (%whitespace-p (%peek parser)) do (%next parser)))

(defun %name-start-p (c)
  (and (characterp c) (or (alpha-char-p c) (char= c #\_) (char= c #\:))))

(defun %name-char-p (c)
  (and (characterp c) (or (alphanumericp c) (find c "._-:"))))

(defun %check-len (parser n)
  (let ((max (%max-string-length parser)))
    (when (and max (> n max))
      (%fail parser 'xml-limit-error
             (format nil "string longer than max-string-length ~A" max)))))

(defun %read-name (parser)
  (unless (%name-start-p (%peek parser))
    (%fail parser 'xml-well-formed-error "expected name"))
  (let ((chars '()))
    (loop for c = (%peek parser)
          while (%name-char-p c)
          do (%check-len parser (1+ (length chars)))
             (push (%next parser) chars))
    (coerce (nreverse chars) 'string)))

(defun %read-quoted (parser)
  (let ((q (%next parser)))
    (unless (or (eql q #\") (eql q #\'))
      (%fail parser 'xml-well-formed-error "expected quoted value"))
    (let ((chars '()))
      (loop for c = (%peek parser)
            do (when (eq c :eof)
                 (%fail parser 'xml-well-formed-error "unterminated quoted value"))
               (when (char= c q)
                 (%next parser)
                 (return))
               (if (char= c #\&)
                   (let ((ent (%read-entity parser)))
                     (%check-len parser (+ (length chars) (length ent)))
                     (setf chars (nconc (nreverse (coerce ent 'list)) chars)))
                   (progn
                     (%check-len parser (1+ (length chars)))
                     (push (%next parser) chars))))
      (coerce (nreverse chars) 'string))))

(defun %read-entity (parser)
  (unless (eql (%next parser) #\&)
    (%fail parser 'xml-well-formed-error "expected '&'"))
  (if (eql (%peek parser) #\#)
      (progn
        (%next parser)
        (let ((hex (eql (%peek parser) #\x)))
          (when hex (%next parser))
          (let ((digits '()))
            (loop for c = (%peek parser)
                  until (or (eq c :eof) (char= c #\;))
                  do (push (%next parser) digits))
            (unless (eql (%next parser) #\;)
              (%fail parser 'xml-well-formed-error "unterminated character reference"))
            (let ((code (ignore-errors
                         (parse-integer (coerce (nreverse digits) 'string)
                                        :radix (if hex 16 10)))))
              (unless code
                (%fail parser 'xml-well-formed-error "bad character reference"))
              (string (code-char code))))))
      (let ((name-chars '()))
        (loop for c = (%peek parser)
              until (or (eq c :eof) (char= c #\;))
              do (push (%next parser) name-chars))
        (unless (eql (%next parser) #\;)
          (%fail parser 'xml-entity-error "unterminated entity"))
        (let ((name (coerce (nreverse name-chars) 'string)))
          (cond
            ((string= name "amp") "&")
            ((string= name "lt") "<")
            ((string= name "gt") ">")
            ((string= name "quot") "\"")
            ((string= name "apos") "'")
            (t
             (restart-case
                 (%fail parser 'xml-entity-error
                        (format nil "unknown entity &~A;" name))
               (continue () :report "Skip the entity" ""))))))))

(defun %read-attrs (parser)
  (let ((acc '()))
    (loop
      (%skip-ws parser)
      (let ((c (%peek parser)))
        (when (or (eq c :eof) (char= c #\/) (char= c #\>) (char= c #\?))
          (return (nreverse acc)))
        (let ((name (%read-name parser)))
          (%skip-ws parser)
          (unless (eql (%next parser) #\=)
            (%fail parser 'xml-well-formed-error "expected '=' in attribute"))
          (%skip-ws parser)
          (push (cons name (%read-quoted parser)) acc))))))

(defun %lookup-prefix (parser prefix)
  (dolist (frame (%ns-stack parser))
    (let ((pair (assoc prefix frame
                       :test (lambda (a b)
                               (cond ((null a) (null b))
                                     ((null b) nil)
                                     (t (string= a b)))))))
      (when pair (return-from %lookup-prefix (cdr pair)))))
  nil)

(defun %push-ns (parser attrs)
  (let ((frame (copy-list (first (%ns-stack parser)))))
    (dolist (pair attrs)
      (let ((name (car pair)) (uri (cdr pair)))
        (cond
          ((string= name "xmlns")
           (setf frame (acons nil uri (remove nil frame :key #'car))))
          ((and (> (length name) 6) (string= name "xmlns:" :end1 6))
           (let ((p (subseq name 6)))
             (setf frame (acons p uri (remove p frame :key #'car :test #'equal))))))))
    (push frame (%ns-stack parser))))

(defun %pop-ns (parser)
  (pop (%ns-stack parser)))

(defun %resolve-name (parser qname &key attribute)
  (multiple-value-bind (prefix local) (xml-protocol::split-qname qname)
    (let ((uri (cond
                 ((and prefix (string= prefix "xmlns")) *xmlns-ns*)
                 (prefix
                  (or (%lookup-prefix parser prefix)
                      (%fail parser 'xml-namespace-error
                             (format nil "unbound prefix ~S" prefix))))
                 (attribute nil)
                 (t (%lookup-prefix parser nil)))))
      (values prefix local uri))))

(defun %unique-attrs (parser attrs)
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (a attrs)
      (let ((key (cons (or (xml-attribute-namespace-uri a) "")
                       (xml-attribute-local-name a))))
        (when (gethash key seen)
          (%fail parser 'xml-well-formed-error
                 (format nil "duplicate attribute ~S" (xml-qname a))))
        (setf (gethash key seen) t)))))

(defun %attrs-from-alist (parser alist)
  (let ((objs '()) (decls '()))
    (dolist (pair alist)
      (let ((name (car pair)) (value (cdr pair)))
        (cond
          ((string= name "xmlns")
           (push (make-instance 'xml-namespace :prefix nil :uri value) decls)
           (push (make-instance 'xml-attribute
                                :local-name "xmlns" :namespace-uri *xmlns-ns*
                                :value value)
                 objs))
          ((and (> (length name) 6) (string= name "xmlns:" :end1 6))
           (let ((p (subseq name 6)))
             (push (make-instance 'xml-namespace :prefix p :uri value) decls)
             (push (make-instance 'xml-attribute
                                 :prefix "xmlns" :local-name p
                                 :namespace-uri *xmlns-ns* :value value)
                   objs)))
          (t
           (multiple-value-bind (prefix local uri)
               (%resolve-name parser name :attribute t)
             (push (make-instance 'xml-attribute
                                  :prefix prefix :local-name local
                                  :namespace-uri uri :value value)
                   objs))))))
    (values (nreverse objs) (nreverse decls))))

(defun %read-until (parser lit)
  (let ((n (length lit))
        (chars '()))
    (loop
      (when (%eof-p parser)
        (%fail parser 'xml-well-formed-error (format nil "unterminated ~S" lit)))
      (let ((ok t)
            (taken '()))
        (loop for i from 0 below n
              for c = (%peek parser)
              do (cond
                   ((or (eq c :eof) (char/= c (char lit i)))
                    (setf ok nil)
                    (return))
                   (t (push (%next parser) taken))))
        (if ok
            (return (coerce (nreverse chars) 'string))
            (progn
              (dolist (c taken)
                (%unread-char parser c))
              (%check-len parser (1+ (length chars)))
              (push (%next parser) chars)))))))

(defun %scan-internal-dtd (parser)
  (loop
    (when (%eof-p parser)
      (%fail parser 'xml-well-formed-error "unterminated DTD"))
    (let ((c (%peek parser)))
      (cond
        ((char= c #\')
         (%next parser)
         (loop for x = (%next parser) until (or (eq x :eof) (char= x #\'))))
        ((char= c #\")
         (%next parser)
         (loop for x = (%next parser) until (or (eq x :eof) (char= x #\"))))
        ((char= c #\<)
         (%next parser)
         (when (loop for ch across "!ENTITY"
                     always (eql (%peek parser) ch)
                     do (%next parser))
           (%skip-ws parser)
           (when (eql (%peek parser) #\%)
             (%next parser) (%skip-ws parser))
           (when (%name-start-p (%peek parser))
             (%read-name parser))
           (%skip-ws parser)
           (let ((ext (or (loop for ch across "SYSTEM"
                                always (eql (%peek parser) ch)
                                do (%next parser))
                          (loop for ch across "PUBLIC"
                                always (eql (%peek parser) ch)
                                do (%next parser)))))
             (when ext
               (restart-case
                   (%fail parser 'xml-entity-error "external entity declaration")
                 (continue () :report "Skip the external entity" nil)))))
         (loop for x = (%peek parser)
               until (or (eq x :eof) (char= x #\>))
               do (%next parser))
         (when (eql (%peek parser) #\>) (%next parser)))
        ((char= c #\])
         (%next parser)
         (return))
        (t (%next parser))))))

(defun %read-doctype (parser)
  ;; caller consumed "<!"
  (loop for ch across "DOCTYPE"
        unless (eql (%next parser) ch)
          do (%fail parser 'xml-well-formed-error "expected <!DOCTYPE"))
  (%skip-ws parser)
  (let ((name (%read-name parser))
        (public nil)
        (system nil))
    (%skip-ws parser)
    (flet ((lit (s)
             (loop for ch across s
                   always (eql (%peek parser) ch)
                   do (%next parser))))
      (cond
        ((lit "PUBLIC")
         (%skip-ws parser) (setf public (%read-quoted parser))
         (%skip-ws parser) (setf system (%read-quoted parser)))
        ((lit "SYSTEM")
         (%skip-ws parser) (setf system (%read-quoted parser)))))
    (when system
      (restart-case
          (%fail parser 'xml-entity-error "external DTD subset is not fetched")
        (continue () :report "Record identifiers and continue" nil)))
    (%skip-ws parser)
    (when (eql (%peek parser) #\[)
      (%next parser)
      (%scan-internal-dtd parser))
    (%skip-ws parser)
    (unless (eql (%next parser) #\>)
      (%fail parser 'xml-well-formed-error "expected '>' after DOCTYPE"))
    (make-instance 'xml-doctype :name name :public-id public :system-id system)))

(defun %read-pi-body (parser)
  ;; caller consumed "<?"
  (let ((target (%read-name parser)))
    (when (string-equal target "xml")
      (%fail parser 'xml-well-formed-error "XML declaration is only allowed at the start"))
    (%skip-ws parser)
    (make-instance 'xml-pi :target target :data (%read-until parser "?>"))))

(defun %read-xmldecl (parser)
  ;; at "<?xml" already consumed "<?" and name xml? handled in dispatch
  (let ((attrs (%read-attrs parser))
        (version "1.0") (encoding "UTF-8") (standalone nil))
    (%skip-ws parser)
    (unless (and (eql (%next parser) #\?) (eql (%next parser) #\>))
      (%fail parser 'xml-well-formed-error "unterminated XML declaration"))
    (dolist (pair attrs)
      (cond
        ((string-equal (car pair) "version") (setf version (cdr pair)))
        ((string-equal (car pair) "encoding") (setf encoding (cdr pair)))
        ((string-equal (car pair) "standalone")
         (setf standalone (string-equal (cdr pair) "yes")))))
    (values version encoding standalone)))

(defun %read-comment (parser)
  ;; caller consumed "<!--"
  (let ((data (%read-until parser "-->")))
    (when (search "--" data)
      (%fail parser 'xml-well-formed-error "comment must not contain '--'"))
    (make-instance 'xml-comment :data data)))

(defun %read-cdata (parser)
  ;; caller consumed "<![CDATA["
  (make-instance 'xml-cdata :data (%read-until parser "]]>")))

(defun %read-start-tag (parser)
  ;; caller consumed "<" and next is a name start
  (let ((qname (%read-name parser))
        (raw-attrs (%read-attrs parser)))
    (%skip-ws parser)
    (let ((empty (eql (%peek parser) #\/)))
      (when empty (%next parser))
      (unless (eql (%next parser) #\>)
        (%fail parser 'xml-well-formed-error "malformed start tag"))
      (%push-ns parser raw-attrs)
      (multiple-value-bind (prefix local uri) (%resolve-name parser qname)
        (multiple-value-bind (attrs decls) (%attrs-from-alist parser raw-attrs)
          (%unique-attrs parser attrs)
          (let ((max (%max-depth parser))
                (depth (1+ (%depth parser))))
            (when (and max (> depth max))
              (%fail parser 'xml-limit-error
                     (format nil "element depth ~A exceeds max-depth ~A" depth max)))
            (setf (%depth parser) depth)
            (push (list qname local uri empty) (%stack parser))
            (values (make-instance 'xml-element
                                   :prefix prefix :local-name local
                                   :namespace-uri uri
                                   :attributes attrs
                                   :namespace-decls decls)
                    empty)))))))

(defun %read-end-tag (parser)
  ;; caller consumed "</"
  (let ((qname (%read-name parser)))
    (%skip-ws parser)
    (unless (eql (%next parser) #\>)
      (%fail parser 'xml-well-formed-error "expected '>' in end tag"))
    (let ((top (pop (%stack parser))))
      (unless top
        (%fail parser 'xml-well-formed-error
               (format nil "unmatched end tag ~S" qname)))
      (destructuring-bind (start-qname start-local start-uri empty) top
        (declare (ignore empty))
        (multiple-value-bind (prefix local uri) (%resolve-name parser qname)
          (unless (and (string= local start-local) (equal uri start-uri))
            (%fail parser 'xml-well-formed-error
                   (format nil "end tag ~S vs ~S" qname start-qname)))
          (decf (%depth parser))
          (%pop-ns parser)
          (make-instance 'xml-element
                         :prefix prefix :local-name local :namespace-uri uri))))))

(defun %read-text (parser)
  (let ((chars '()))
    (loop for c = (%peek parser)
          until (or (eq c :eof) (char= c #\<))
          do (if (char= c #\&)
                 (let ((ent (%read-entity parser)))
                   (%check-len parser (+ (length chars) (length ent)))
                   (setf chars (nconc (nreverse (coerce ent 'list)) chars)))
                 (progn
                   (%check-len parser (1+ (length chars)))
                   (push (%next parser) chars))))
    (make-instance 'xml-text :data (coerce (nreverse chars) 'string))))

(defun %match (parser string)
  (loop for ch across string
        always (eql (%peek parser) ch)
        do (%next parser)))

(defun %ensure-buf (parser n)
  (loop while (< (length (%buf parser)) n)
        do (let ((c (read-char (%in parser) nil :eof)))
             (setf (%buf parser) (append (%buf parser) (list c)))
             (when (eq c :eof) (return)))))

(defun %looking-at (parser string)
  (%ensure-buf parser (length string))
  (loop for i from 0 below (length string)
        for c in (%buf parser)
        always (eql c (char string i))))

(defun %emit-start-doc (parser &optional version encoding standalone)
  (setf (%emitted-start-doc parser) t
        (%phase parser) :misc)
  (values :start-document
          (make-xml-document :version (or version "1.0")
                             :encoding (or encoding "UTF-8")
                             :standalone standalone)))

(defun %consume-lt (parser)
  (unless (eql (%next parser) #\<)
    (%fail parser 'xml-well-formed-error "expected '<'")))

(defun %parse-markup (parser)
  "Stream is at '<'."
  (%consume-lt parser)
  (let ((c (%peek parser)))
    (cond
      ((eq c :eof)
       (%fail parser 'xml-well-formed-error "truncated markup"))
      ((char= c #\/)
       (%next parser)
       (values :end-element (%read-end-tag parser)))
      ((char= c #\?)
       (%next parser)
       (let ((target (%read-name parser)))
         (cond
           ((and (string-equal target "xml") (not (%emitted-start-doc parser)))
            (multiple-value-bind (v e s) (%read-xmldecl parser)
              (%emit-start-doc parser v e s)))
           ((string-equal target "xml")
            (%fail parser 'xml-well-formed-error
                   "XML declaration is only allowed at the start"))
           (t
            (%skip-ws parser)
            (values :processing-instruction
                    (make-instance 'xml-pi :target target
                                   :data (%read-until parser "?>")))))))
      ((char= c #\!)
       (%next parser)
       (cond
         ((%match parser "--")
          (values :comment (%read-comment parser)))
         ((%match parser "[CDATA[")
          (unless (%stack parser)
            (%fail parser 'xml-well-formed-error "CDATA outside root"))
          (values :characters (%read-cdata parser)))
         ((%match parser "DOCTYPE")
          (when (%stack parser)
            (%fail parser 'xml-well-formed-error "DOCTYPE inside element"))
          ;; %read-doctype expects to read DOCTYPE itself — we already consumed it
          (%skip-ws parser)
          (let ((name (%read-name parser))
                (public nil) (system nil))
            (%skip-ws parser)
            (cond
              ((%match parser "PUBLIC")
               (%skip-ws parser) (setf public (%read-quoted parser))
               (%skip-ws parser) (setf system (%read-quoted parser)))
              ((%match parser "SYSTEM")
               (%skip-ws parser) (setf system (%read-quoted parser))))
            (when system
              (restart-case
                  (%fail parser 'xml-entity-error "external DTD subset is not fetched")
                (continue () :report "Record identifiers and continue" nil)))
            (%skip-ws parser)
            (when (eql (%peek parser) #\[)
              (%next parser)
              (%scan-internal-dtd parser))
            (%skip-ws parser)
            (unless (eql (%next parser) #\>)
              (%fail parser 'xml-well-formed-error "expected '>' after DOCTYPE"))
            (values :dtd (make-instance 'xml-doctype
                                        :name name :public-id public :system-id system))))
         (t (%fail parser 'xml-well-formed-error "malformed '<!' markup"))))
      (t
       (multiple-value-bind (el empty) (%read-start-tag parser)
         (when empty
           (setf (%pending parser)
                 (append (%pending parser)
                         (list (cons :end-element
                                     (let ((end (%read-end-tag-empty parser el)))
                                       end))))))
         (values :start-element el))))))

(defun %read-end-tag-empty (parser el)
  (let ((top (pop (%stack parser))))
    (declare (ignore top))
    (decf (%depth parser))
    (%pop-ns parser)
    (make-instance 'xml-element
                   :prefix (xml-element-prefix el)
                   :local-name (xml-element-local-name el)
                   :namespace-uri (xml-element-namespace-uri el))))

(defun %next-event (parser)
  (when (%pending parser)
    (let ((pair (pop (%pending parser))))
      (return-from %next-event (values (car pair) (cdr pair)))))
  (when (eq (%phase parser) :done)
    (return-from %next-event (values nil nil)))
  (unless (%emitted-start-doc parser)
    (%skip-ws parser)
    (if (and (%looking-at parser "<?xml")
             (progn (%ensure-buf parser 6)
                    (let ((c (nth 5 (%buf parser))))
                      (or (%whitespace-p c) (eql c #\?)))))
        (return-from %next-event (%parse-markup parser))
        (return-from %next-event (%emit-start-doc parser))))
  (when (null (%stack parser))
    (%skip-ws parser))
  (cond
    ((%eof-p parser)
     (when (%stack parser)
       (%fail parser 'xml-well-formed-error "unclosed element"))
     (setf (%phase parser) :done)
     (values :end-document nil))
    ((eql (%peek parser) #\<)
     (%parse-markup parser))
    ((null (%stack parser))
     (%fail parser 'xml-well-formed-error "text outside root element"))
    (t
     (values :characters (%read-text parser)))))

(defun %open-source (source)
  (etypecase source
    (stream (values source (constantly nil)))
    (string
     (let ((s (make-string-input-stream source)))
       (values s (lambda () (close s)))))
    (pathname
     (let ((s (open source :direction :input :external-format :utf-8)))
       (values s (lambda () (close s)))))
    ((vector (unsigned-byte 8))
     (let ((s (make-string-input-stream
               (babel:octets-to-string source :encoding :utf-8))))
       (values s (lambda () (close s)))))))

(defun make-native-parser (source &key max-depth max-string-length backend)
  (multiple-value-bind (in close) (%open-source source)
    (let ((p (make-instance 'native-event-parser
                            :backend backend
                            :source source
                            :in in
                            :close close
                            :max-depth max-depth
                            :max-string-length max-string-length)))
      (%init-ns p)
      p)))

(defun %build-element (parser start)
  (let ((kids '()))
    (loop
      (multiple-value-bind (ev val) (%next-event parser)
        (cond
          ((null ev)
           (%fail parser 'xml-well-formed-error "unclosed element"))
          ((eq ev :end-element)
           (setf (xml-element-children start) (nreverse kids))
           (return start))
          ((eq ev :start-element)
           (push (%build-element parser val) kids))
          ((eq ev :characters)
           (push val kids))
          ((eq ev :comment)
           (push val kids))
          ((eq ev :processing-instruction)
           (push val kids))
          (t
           (%fail parser 'xml-well-formed-error
                  (format nil "unexpected ~S inside element" ev))))))))

(defun parse-document-from (parser)
  (let ((doc nil)
        (kids '()))
    (loop
      (multiple-value-bind (ev val) (%next-event parser)
        (cond
          ((null ev)
           (unless doc
             (%fail parser 'xml-well-formed-error "empty document"))
           (setf (xml-document-children doc) (nreverse kids))
           (return doc))
          ((eq ev :start-document)
           (setf doc val))
          ((eq ev :end-document)
           (unless doc (setf doc (make-xml-document)))
           (setf (xml-document-children doc) (nreverse kids))
           (return doc))
          ((eq ev :start-element)
           (push (%build-element parser val) kids))
          ((or (eq ev :comment) (eq ev :processing-instruction) (eq ev :dtd)
               (eq ev :characters))
           (push val kids))
          (t
           (%fail parser 'xml-well-formed-error
                  (format nil "unexpected ~S at document level" ev))))))))
