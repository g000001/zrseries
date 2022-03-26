;;; -*- Mode: lisp -*-

(defpackage #:zrseries-system
  (:use #:common-lisp #:asdf))

(in-package #:zrseries-system)

(asdf:defsystem zrseries
    :description "See <http://series.sourceforge.net/>."
    :author "Richard C. Waters"
    :maintainer "See <http://series.sourceforge.net/>."
    :licence "MIT"
    :version "2.2.11"
    :serial t
    :components ((:file "s-package")
                 (:file "s-code")))

(defmethod perform ((op test-op) (c (eql (find-system :zrseries))))
  (oos 'test-op 'zrseries-tests))

(asdf:defsystem zrseries-tests
  :depends-on (zrseries)
  :version "2.2.11"			; Same as zrseries
  :in-order-to ((test-op (load-op :zrseries)))
  :components
  ((:file "s-test")))

(defmethod operation-done-p ((op test-op)
                             (c (eql (find-system :zrseries-tests))))
  nil)


(defmethod perform ((op test-op) (c (eql (find-system :zrseries-tests))))
  (or (funcall (intern "DO-TESTS" (find-package "RT")))
      (error "TEST-OP failed for ZRSERIES-TESTS")))
