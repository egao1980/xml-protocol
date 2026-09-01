(defpackage #:xml-protocol
  (:use #:cl)
  (:nicknames #:stack-xml)
  (:export #:xml-error
           #:xml-parse-error
           #:xml-encode-error
           #:xml-well-formed-error
           #:xml-namespace-error
           #:xml-entity-error
           #:xml-limit-error
           #:xml-error-message
           #:xml-error-line
           #:xml-error-column

           #:*xml-backend*
           #:xml-backend
           #:backend-encode
           #:backend-decode
           #:encode
           #:decode
           #:encode-to-octets
           #:decode-octets

           #:xml-node
           #:xml-document
           #:xml-document-p
           #:xml-document-version
           #:xml-document-encoding
           #:xml-document-standalone
           #:xml-document-children
           #:xml-element
           #:xml-element-p
           #:xml-element-local-name
           #:xml-element-prefix
           #:xml-element-namespace-uri
           #:xml-element-attributes
           #:xml-element-namespace-decls
           #:xml-element-children
           #:xml-attribute
           #:xml-attribute-p
           #:xml-attribute-local-name
           #:xml-attribute-prefix
           #:xml-attribute-namespace-uri
           #:xml-attribute-value
           #:xml-attribute-specified-p
           #:xml-namespace
           #:xml-namespace-p
           #:xml-namespace-prefix
           #:xml-namespace-uri
           #:xml-text
           #:xml-text-p
           #:xml-text-data
           #:xml-cdata
           #:xml-cdata-p
           #:xml-cdata-data
           #:xml-comment
           #:xml-comment-p
           #:xml-comment-data
           #:xml-pi
           #:xml-pi-p
           #:xml-pi-target
           #:xml-pi-data
           #:xml-doctype
           #:xml-doctype-p
           #:xml-doctype-name
           #:xml-doctype-public-id
           #:xml-doctype-system-id

           #:make-xml-element
           #:make-xml-document
           #:xml-qname
           #:xml-local-name
           #:xml-attr
           #:xml-child
           #:xml-children-named
           #:xml-element-text
           #:xml-named-p
           #:document-root-element

           #:xml-event-writer
           #:event-writer-backend
           #:event-writer-stream
           #:backend-make-event-writer
           #:write-event
           #:make-event-writer
           #:with-event-writer

           #:node-from-sexp
           #:node-to-sexp

           #:install-http-xml-hooks
           #:install-serdes-xml-hooks
           #:xml-serdes-backend
           #:make-xml-serdes-backend
           #:use-xml-serdes-backend))

(in-package #:xml-protocol)
