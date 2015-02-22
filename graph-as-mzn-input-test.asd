;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :asdf-user)

(defsystem :graph-as-mzn-input-test
  :name "graph-as-mzn-input-test"
  :description "Tests for graph-as-mzn-input"
  :components ((:module "test"
                :components ((:file "package")
                             (:file "test" :depends-on ("package")))))
  :depends-on (:graph-as-mzn-input :myam :alexandria))

(defmethod perform ((op test-op)
                    (system (eql (find-system :graph-as-mzn-input-test))))
  (perform op (find-system :graph-as-mzn-input)))
