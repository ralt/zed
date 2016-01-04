(in-package #:zed)

(defvar *commands* (make-hash-table :test #'equal)
  "Hash table of commands in the following form:
{
  <command>: (:lambda <function>
              :docstring <docstring>)
}")

(defmacro defcommand (name args docstring &body body)
  "Define a new command with its docstring."
  `(setf (gethash (string-downcase (symbol-name (quote ,name))) *commands*)
         (list :lambda (lambda ,args
                         ,@body)
               :docstring ,docstring)))
