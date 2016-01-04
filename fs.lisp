(in-package #:zed)

;;; fs helpers

(defun touch (path)
  (open path :direction :probe :if-does-not-exist :create))
