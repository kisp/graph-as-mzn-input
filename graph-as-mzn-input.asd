;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :asdf-user)

(defsystem :graph-as-mzn-input
  :name "graph-as-mzn-input"
  :description "Solve constraints based on an input graph using MiniZinc."
  :author "Kilian Sprotte <kilian.sprotte@gmail.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :components ((:static-file "version" :pathname #p"version.lisp-expr")
               (:file "package")
               (:file "graph-as-mzn-input" :depends-on ("package"))
               )
  :depends-on (:alexandria :graph :cl-ppcre))

(defmethod perform ((op test-op)
                    (system (eql (find-system :graph-as-mzn-input))))
  (oos 'load-op :graph-as-mzn-input-test)
  (funcall (intern "RUN!" "MYAM") :graph-as-mzn-input-test))
