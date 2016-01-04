(in-package #:zed)

(defun main (args)
  (if (second args)
      (multiple-value-bind
            (list present-p)
          (gethash (second args) *commands*)
        (if present-p
            (handler-case
                (apply (getf list :lambda) (rest (rest args)))
              (sb-int:simple-program-error () (usage)))
            (usage)))
      (usage)))

(defun usage ()
  (format t "usage: git zed <COMMAND> [OPTIONS]~%"))

(defcommand create (title)
  "Create an issue.")
