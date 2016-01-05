(in-package #:zed)

(defclass git-object ()
  ((hash :accessor hash :type string)))

(defclass git-blob (git-object)
  ((content :initarg :content :reader content :type string)))

(deftype git-tree-type ()
  '(member tree blob))

(defclass git-tree-file ()
  ((mode :initarg :mode :type string :reader mode)
   (hash :initarg :hash :type string :reader hash)
   (name :initarg :name :type string :reader name)
   (type :initarg :type :type git-tree-type :reader tree-type)))

(defclass git-tree (git-object)
  ((files :initarg files :type '(vector git-tree-file))))

(defun files (tree)
  (let ((files-list nil))
    (multiple-value-prog1 files-list
      (loop for file across (slot-value tree 'files)
         do (progn
              (push (mode tree) files-list)
              (push (string-downcase (symbol-name (tree-type tree))) files-list)
              (push (hash tree) files-list)
              (push (name tree) files-list))))))

(defgeneric save (object)
  (:documentation "Saves an object in git database."))

(defmethod save ((object git-object))
  (error "Not implemented"))

(defmethod save ((tree git-tree))
  (setf (hash tree) (run-with-input (format-tree tree) "git mktree")))

(defmethod save ((blob git-blob))
  (setf (hash blob) (run-with-input (content blob) "git hash-object -w --stdin")))

(defun format-tree (tree)
  (format nil
          "~{~A ~A ~A~T~A~%~}"
          (files tree)))
