(in-package #:zed)

(defclass issue-message-tree (git-tree)
  ((author :initarg :author :reader author :type string)
   (date :initarg :date :reader date :type number)
   (content :initarg :content :reader content :type string)
   (children :initarg :children :reader children :type (vector issue-message-tree)
             :initform (make-array
                        0
                        :adjustable t
                        :fill-pointer 0
                        :element-type 'issue-message-tree))))

(deftype issue-status ()
  '(member open closed))

(defclass issue-tree (git-tree)
  ((title :initarg :title :reader title :type string)
   (status :initarg :status :reader status :type issue-status)
   (head :initarg :head :reader head :type issue-message-tree)))

(defclass issues-list-tree (git-tree)
  ((issues :type (vector issue-tree) :initform (make-array
                                                0
                                                :adjustable t
                                                :fill-pointer 0
                                                :element-type 'issue-tree)
           :accessor issues)))

(defmethod initialize-instance :after ((tree issues-list-tree) &key)
  "Read all the existing issues and put them in the 'issues' slot"
  (when (probe-file *head-path*)
    (loop for issue in (load-tree (uiop:read-file-line *head-path*))
       ;; Every entry in the top-level tree is a tree,
       ;; so we can just push them.
       do (vector-push-extend issue (issues tree)))))
