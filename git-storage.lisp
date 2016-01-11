(in-package #:zed)

(defclass git-object ()
  ((hash :initarg :hash :accessor hash :type string :initform nil)
   (filename :initarg :filename :type string :initform nil)))

(defclass git-blob (git-object)
  ((content :initarg :content :reader content :type string)))

(defclass git-tree (git-object)
  ((blobs :initarg :blobs :accessor blobs :type (vector git-blob)
          :initform (make-array
                     0
                     :adjustable t
                     :fill-pointer 0))
   (trees :initarg :trees :accessor trees :type (vector git-tree)
          :initform (make-array
                     0
                     :adjustable t
                     :fill-pointer 0))
   (commit-hash :type string :accessor commit-hash)
   ;; See issue-tree class for the reasoning behind this slot.
   (updated :accessor updated :type boolean :initform nil)))

(defun filename (git-object)
  (or (slot-value git-object 'filename)
      (hash git-object)))

(defun save-tree (tree)
  (unless (and (hash tree)
               (null (updated tree)))
    (loop for tree across (trees tree)
       do (save-tree tree))
    (loop for blob across (blobs tree)
       do (save-blob blob))
    (setf (hash tree) (run-with-input (git-print tree) "git mktree"))))

(defun save-blob (blob)
  (setf (hash blob) (run-with-input (content blob) "git hash-object -w --stdin")))

(defun add-blob (tree blob)
  (vector-push-extend blob (blobs tree)))

(defun add-tree (parent-tree tree)
  (vector-push-extend tree (trees parent-tree)))

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

(defun commit-tree (path tree)
  (setf (commit-hash tree)
        (run "git commit-tree ~A -m 'dummy' ~A"
             (if (probe-file *head-path*)
                 ;; We have a parent! Take that, Batman!
                 (format nil "-p ~A" (uiop:read-file-line path))
                 ;; For I am The First
                 "")
             (hash tree)))
  (ensure-directories-exist (uiop:pathname-directory-pathname path))
  (with-open-file (f path
                     :direction :output :if-exists :overwrite
                     :if-does-not-exist :create)
    (let ((buf (uiop:strcat (commit-hash issues-list) #\Newline)))
      (write-sequence buf f))))

(defun load-tree (hash)
  (mapcar
   (lambda (line)
     (make-instance 'issue-tree
                    :hash (second (uiop:split-string line :separator #(#\Tab)))))
   (uiop:split-string (run "git ls-tree ~A" hash) :separator #(#\Newline))))

(defun long-hash (short-hash)
  (run "git rev-parse ~A" short-hash))
