;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :graph-as-mzn-input-test)

(defsuite* :graph-as-mzn-input-test)

(defun check-format-graph (expected nodes edges)
  (is (string= (string-trim '(#\space #\newline) expected)
               (string-trim
                '(#\space #\newline)
                (with-output-to-string (output)
                  (format-graph
                   (populate (make-instance 'digraph)
                             :nodes nodes
                             :edges edges)
                   output
                   nodes))))))

(defun sequal (a b)
  (set-equal a b :test #'set-equal))

(deftest test.1
  (check-format-graph
   "nnode = 3;
adjMatrix = [|
  0,0,0
| 0,0,1
| 0,0,0
|];"
   '(1 2 3)
   '((2 3))))

(deftest test.2
  (check-format-graph
   "nnode = 3;
adjMatrix = [|
  1,0,0
| 0,0,1
| 0,0,0
|];"
   '(1 2 3)
   '((2 3) (1 1))))

(deftest test.3
  (check-format-graph
   "nnode = 2;
adjMatrix = [|
  0,0
| 1,0
|];"
   '(1 2)
   '((2 1))))

(deftest test.4
  (let ((graph (populate (make-instance 'digraph) :nodes '(1 2))))
    (is (sequal '(nil (1) (2) (1 2))
                (solve-all graph)))))

(deftest test.5
  (let ((graph (populate (make-instance 'digraph) :nodes '(1))))
    (is (sequal '(nil (1))
                (solve-all graph)))))

(deftest test.6
  (let ((graph (populate (make-instance 'digraph) :nodes '(a b c))))
    (is (sequal '(nil (a) (b) (c) (a c) (a b) (b c) (a b c))
                (solve-all graph)))))

(deftest test.7
  (let ((graph (populate (make-instance 'digraph)
                         :nodes '(a b c)
                         :edges '((a b) (b c)))))
    (is (sequal '(nil (a) (b) (c) (a c))
                (solve-all graph
                           :constraints
                           '("
% conflict-free
constraint forall(a,b in argumentsRange where adjMatrix[a,b] == 1)
                      (arguments[a] = false \\/ arguments[b] = false);
"))))))

(deftest test.8
  (let ((graph (populate (make-instance 'digraph)
                         :nodes '(a b c)
                         :edges '((a b) (b c)))))
    (is (sequal '(nil (a) (a c))
                (solve-all graph
                           :constraints
                           '("
% conflict-free
constraint forall(a,b in argumentsRange where adjMatrix[a,b] == 1)
                      (arguments[a] = false \\/ arguments[b] = false);
"
                             "
%admissible
constraint forall(b,c in argumentsRange where adjMatrix[b,c] == 1)
                     (arguments[c] -> exists(a in argumentsRange where adjMatrix[a,b]
                       == 1)(arguments[a]));
"))))))

(deftest test.9
  (let ((graph (populate (make-instance 'digraph)
                         :nodes '(a b c)
                         :edges '((a b) (b c)))))
    (is (sequal '((a c))
                (solve-all graph
                           :constraints
                           '("
% conflict-free
constraint forall(a,b in argumentsRange where adjMatrix[a,b] == 1)
                      (arguments[a] = false \\/ arguments[b] = false);
"
                             "
%admissible
constraint forall(b,c in argumentsRange where adjMatrix[b,c] == 1)
                     (arguments[c] -> exists(a in argumentsRange where adjMatrix[a,b]
                       == 1)(arguments[a]));
"

                             "
%complete
constraint forall(b in argumentsRange)(if sum(a in argumentsRange where adjMatrix[a,b] == 1)
                    (adjMatrix[a,b]) == 0 then arguments[b] else true endif);

"

                             "
constraint forall(a,b in argumentsRange where adjMatrix[a,b] == 1)
           (arguments[a] -> forall (c in argumentsRange where adjMatrix[b,c]
                         == 1)((adjMatrix[a,c] != 1 /\\ adjMatrix[c,a] != 1 /\\
                           sum(d in argumentsRange where adjMatrix[d,c] == 1)
                             (adjMatrix[d,c]) == 1) -> arguments[c]));
"))))))
