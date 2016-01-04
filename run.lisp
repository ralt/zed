(in-package #:zed)

(defun run (command &rest args)
  (handler-case
      (uiop:run-program (apply #'format nil command args)
                        :output '(:string :stripped t))
    (uiop:subprocess-error () "")))

(defun run-with-input (input command &rest args)
  (handler-case
      (uiop:run-program (apply #'format nil command args)
                        :output '(:string :stripped t)
                        :input (make-string-input-stream input))
    (uiop:subprocess-error () "")))
