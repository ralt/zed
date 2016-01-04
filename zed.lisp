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

(defun l (priority text &rest args)
  (syslog:log "zed" :local0 priority (apply #'format nil text args)))

(defun usage ()
  (format t "usage: git zed <COMMAND> [ARGS]~%"))

(defcommand create (title)
  "Create an issue."
  (let ((zed-msg-file (git-root ".git/ZED_MSG")))
    (touch zed-msg-file)
    (launch-editor zed-msg-file)))
