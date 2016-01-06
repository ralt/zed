(in-package #:zed)

(defclass git-object ()
  ((hash :initarg :hash :accessor hash :type string :initform nil)
   (filename :initarg :filename :type string :initform nil)))

(defclass git-blob (git-object)
  ((content :initarg :content :reader content :type string)))

(defclass git-tree (git-object)
  ((blobs :initarg :blobs :accessor blobs :type (vector git-blob) :initform (make-array
                                                                             0
                                                                             :adjustable t
                                                                             :fill-pointer 0))
   (trees :initarg :trees :accessor trees :type (vector git-tree) :initform (make-array
                                                                             0
                                                                             :adjustable t
                                                                             :fill-pointer 0))))

(defun filename (git-object)
  (or (slot-value git-object 'filename)
      (hash git-object)))

(defun save-tree (tree)
  (loop for tree across (trees tree)
     do (save-tree tree)h)
  (loop for blob across (blobs tree)
     do (save-blob blob))
  (setf (hash tree) (run-with-input (git-print tree) "git mktree")))

(defun save-blob (blob)
  (setf (hash blob) (run-with-input (content blob) "git hash-object -w --stdin")))

(defgeneric git-print (git-object)
  (:documentation "Prints an object for git storage."))

(defmethod git-print ((tree git-tree))
  (format nil
          "~{~A~^~%~}"
          (append
           (loop for blob across (blobs tree)
              collect (git-print blob))
           (loop for tree across (trees tree)
              collect (git-print-in-tree tree)))))

(defun git-print-in-tree (tree)
  (format nil "~A ~A ~A~A~A"
          "040000" "tree"
          (hash tree) #\Tab (filename tree)))

(defmethod git-print ((blob git-blob))
  (format nil "~A ~A ~A~A~A"
          "100644" "blob"
          (hash blob) #\Tab (filename blob)))
