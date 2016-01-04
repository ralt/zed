(in-package #:zed)

(defun run (command &rest args)
  (handler-case
      (let ((output (with-output-to-string (out)
                      (uiop:run-program (apply #'format nil command args)
                                        :output out)
                      out)))
        ;; Most commands return a superfluous newline, remove it
        (subseq output 0 (1- (length output))))
    (uiop:subprocess-error () "")))

(defun run-with-input (input command &rest args)
  (handler-case
      (let ((output (with-output-to-string (out)
                      (uiop:run-program (apply #'format nil command args)
                                        :output out
                                        :input (make-string-input-stream input))
                      out)))
        ;; Most commands return a superfluous newline, remove it
        (subseq output 0 (1- (length output))))
    (uiop:subprocess-error () nil)))
