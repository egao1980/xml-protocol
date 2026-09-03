(defpackage #:xml-backend-native
  (:use #:cl #:xml-protocol)
  (:export #:native-backend
           #:make-native-backend
           #:use-native-backend))

(in-package #:xml-backend-native)
