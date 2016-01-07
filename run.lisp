(in-package #:zed)

(defun run (command &rest args)
  (uiop:run-program (apply #'format nil command args)
                    :output '(:string :stripped t)))

(defun run-with-input (input command &rest args)
  (uiop:run-program (apply #'format nil command args)
                    :output '(:string :stripped t)
                    :input (make-string-input-stream input)))
