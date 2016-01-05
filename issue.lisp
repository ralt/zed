(in-package #:zed)

(defclass issue-message-tree ()
  ((author :initarg :author :type string)
   (date :initarg :date :type number)
   (content :initarg :content :type string)
   (children :initarg :children :type (vector issue-message-tree))))

(deftype issue-status ()
  '(member open closed))

(defclass issue-tree ()
  ((title :initarg :title :type string)
   (status :initarg :status :type issue-status)
   (head :initarg :head :type issue-message-tree)))

(defclass issues-list-tree ()
  ((issues :type (vector issue-tree) :initform (make-array
                                                0
                                                :adjustable t
                                                :fill-pointer 0
                                                :element-type 'issue-tree)
           :accessor issues)))

(defmethod initialize-instance ((tree issues-list-tree))
  "Read all the existing issues and put them in the 'issues' slot")

(defun issue-create (title content author-name author-email date)
  (let* ((message (make-instance 'issue-message-tree
                                 :author (format nil "~A <~A>"
                                                 author-name author-email)
                                 :date date
                                 :content content))
         (issue (make-instance 'issue-tree
                               :title title
                               :status 'open
                               :head message))
         (issues-list (make-instance 'issues-list-tree)))
    (push (issues issues-list) issue)
    (save issues-list)))
