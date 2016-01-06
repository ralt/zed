(in-package #:zed)

(defclass git-object ()
  ((hash :accessor hash :type string)))

(defclass git-blob (git-object)
  ((content :initarg :content :reader content :type string)))

(deftype git-tree-type ()
  '(member tree blob))

(defclass git-tree (git-object)
  ((blobs :initarg :blobs :accessor blobs :type (vector git-blob))
   (trees :initarg :trees :accessor trees :type (vector git-tree))))

(defgeneric save (object)
  (:documentation "Saves an object in git database."))

(defmethod save ((object git-object))
  (error "Not implemented"))

(defmethod save ((tree git-tree))
  (loop for tree across (trees tree)
     do (save tree))
  (loop for blob across (blobs tree)
     do (save blob))
  (setf (hash tree) (run-with-input (print tree) "git mktree")))

(defmethod save ((blob git-blob))
  (setf (hash blob) (run-with-input (content blob) "git hash-object -w --stdin")))

(defmethod print-object ((tree git-tree) stream)
  (format stream
          "~{~A~%~}"
          (loop for blob in (blobs tree)
             collect (print blob)))
  (format stream
          "~{~A~^~%~}"
          (loop for tree in (trees tree)
             collect (print-in-tree tree stream))))

(defun print-in-tree (tree stream)
  (format stream "~A ~A ~A~T~A"
          "040000" "tree"
          (hash tree) (hash tree)))

(defmethod print-object ((blob git-blob) stream)
  (format stream "~A ~A ~A~T~A"
          "100644" "blob"
          (hash blob) (hash blob)))
