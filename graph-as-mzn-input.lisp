;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :graph-as-mzn-input)

(defun parse-line (string)
  (labels ((rall (string)
             (setq string (ppcre:regex-replace-all "true" string "T"))
             (setq string (ppcre:regex-replace-all "false" string "NIL"))))
    (read-from-string (format nil "(~A)" (rall string)))))

(defun parse-output (stream)
  (loop for line = (read-line stream)
        until (equal line "==========")
        collect (prog1
                    (parse-line line)
                  (assert (equal "----------" (read-line stream))))))

(defun tmp-pathname (base-name)
  (parse-namestring
   (format nil "/tmp/~A-~D-~D" base-name
           #+clisp(posix:process-id)
           #+sbcl (sb-posix:getpid)
           #-(or clisp sbcl) (error "do not know how to getpid")
           (random 1000000))))

(defmacro with-tmp-file ((var base-name) &body body)
  `(let ((,var (tmp-pathname ,base-name)))
     (unwind-protect
          (progn ,@body)
       (when (probe-file ,var)
         (delete-file ,var)))))

(defun format-graph (graph stream &optional (nodes (graph:nodes graph)))
  (let ((order (length nodes)))
    (format stream "nnode = ~D;~%" order)
    (format stream "adjMatrix = [|~%")
    (let ((first-time t))
      (dolist (node-a nodes)
        (format stream "~A~{~A~^,~}~%"
                (if first-time "  " "| ")
                (mapcar (lambda (node-b)
                          (if (graph:has-edge-p graph (list node-a node-b))
                              1
                              0))
                        nodes))
        (setq first-time nil)))
    (format stream "|];~%")))

(defun format-header (output)
  (format output "int : nnode;
array[1..nnode, 1..nnode] of int: adjMatrix;
set of int: argumentsRange = 1..nnode;
array[1..nnode] of var bool: arguments;
"))

(defun format-footer (output)
  (format output "solve :: bool_search(arguments, input_order, indomain_max, complete)
      satisfy;

output [show(arguments[a]) ++ \" \" | a in argumentsRange];
"))

(defun solve-all (graph &key constraints)
  (let ((nodes (graph:nodes graph)))
    (with-tmp-file (tmp-file "graph-mzn")
      (let ((tmp-file (make-pathname :type "mzn" :defaults tmp-file)))
        (with-tmp-file (tmp-file2 "graph-mzn")
          (with-tmp-file (tmp-file3 "graph-mzn-error")
            (with-open-file (output tmp-file :direction :output)
              (format-graph graph output nodes)
              (format-header output)
              (dolist (constraint constraints)
                (format output "~A~%" constraint))
              (format-footer output))
            (let ((process
                    (sb-ext:run-program "mzn-g12fd" (list "-a" (namestring tmp-file))
                                        :search t
                                        :output (namestring tmp-file2)
                                        :error (namestring tmp-file3))))
              (unless (zerop (sb-ext:process-exit-code process))
                (error "mzn-g12fd failed:~%~A" (read-file-into-string tmp-file3)))
              (let ((result
                      (with-open-file (input tmp-file2)
                        (parse-output input))))
                (mapcar (lambda (s)
                          (loop for x in s
                                for i upfrom 0
                                when x
                                  collect (nth i nodes)))
                        result)))))))))
