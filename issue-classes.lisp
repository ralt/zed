(in-package #:zed)

(defvar *head-path* (git-root ".git/refs/zed/issues/head"))

(defclass issue ()
  ((git-tree :initarg :git-tree :reader git-tree :type git-tree :initform nil)))

(defmethod initialize-instance :after ((issue issue) &key)
  (unless (git-tree issue)
    (setf (slot-value issue 'git-tree) (make-instance 'git-tree))))

(defclass issue-message-tree (issue)
  ((author :initarg :author :accessor author :type string)
   (date :initarg :date :accessor date :type number)
   (content :initarg :content :accessor content :type string)
   (children :initarg :children :accessor children :type (vector issue-message-tree)
             :initform (make-array
                        0
                        :adjustable t
                        :fill-pointer 0
                        :element-type 'issue-message-tree))))

(defclass issue-messages-list-tree (issue)
  ((messages :type (vector issue-message-tree)
             :initform (make-array
                        0
                        :adjustable t
                        :fill-pointer 0
                        :element-type 'issue-tree)
             :accessor messages)))

(defmethod initialize-instance :after ((messages-list issue-messages-list-tree) &key)
  "Read all the existing messages and put them in the 'messages' slot"
  (when (git-tree messages-list)
    (loop for message-tree in (load-tree (git-tree messages-list))
       ;; Every entry in the top-level tree is an issue,
       ;; so we can just push them.
       do (vector-push-extend (make-instance 'issue-message-tree
                                             :git-tree message-tree)
                              (messages messages-list)))))

(deftype issue-status ()
  '(member open closed))

(defclass issue-tree (issue)
  ((title :initarg :title :accessor title :type string)
   (status :initarg :status :accessor status :type issue-status)
   (head :initarg :head :accessor head :type issue-message-tree)
   ;; This slot is pretty ugly in itself.
   ;; When an issue is updated (new child), it already has a
   ;; hash, so the issues list would ignore it during the save.
   ;; This slot makes sure it's still updated despite the
   ;; existing hash.
   (updated :accessor updated :initform nil :type boolean)))

(defclass issues-list-tree (issue)
  ((issues :type (vector issue-tree) :initform (make-array
                                                0
                                                :adjustable t
                                                :fill-pointer 0
                                                :element-type 'issue-tree)
           :accessor issues)))

(defmethod initialize-instance :after ((issues-list issues-list-tree) &key)
  "Read all the existing issues and put them in the 'issues' slot"
  (when (probe-file *head-path*)
    (loop for issue-tree in (load-tree (uiop:read-file-line *head-path*))
       ;; Every entry in the top-level tree is an issue,
       ;; so we can just push them.
       do (vector-push-extend (make-instance 'issue-tree
                                             :git-tree issue-tree)
                              (issues issues-list)))))
