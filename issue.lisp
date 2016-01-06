(in-package #:zed)

(defvar *head-path* (git-root ".git/refs/zed/issues/head"))

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

(defgeneric save (object)
  (:documentation "Saves an issue."))

(defmethod save ((message issue-message-tree))
  (vector-push-extend (make-instance 'git-blob
                                     :filename "author"
                                     :content (author message))
                      (blobs message))
  (vector-push-extend (make-instance 'git-blob
                                     :filename "date"
                                     :content (write-to-string (date message)))
                      (blobs message))
  (vector-push-extend (make-instance 'git-blob
                                     :filename "content"
                                     :content (content message))
                      (blobs message))
  (when (> (length (children message)) 0)
    (let ((children-tree (make-instance 'git-tree :filename "children")))
      (loop for child across (children message)
         do (vector-push-extend child (trees children-tree)))
      (push children-tree (trees message))))
  (save-tree message))

(defmethod save ((issue issue-tree))
  (vector-push-extend (make-instance 'git-blob
                                     :filename "title"
                                     :content (title issue))
                      (blobs issue))
  (vector-push-extend (make-instance 'git-blob
                                     :filename "status"
                                     :content (string-downcase
                                               (symbol-name (status issue))))
                      (blobs issue))
  (setf (slot-value (head issue) 'filename) "head")
  (save (head issue))
  (vector-push-extend (head issue) (trees issue))
  (save-tree issue))

(defmethod save ((issues-list issues-list-tree))
  (loop for issue across (issues issues-list)
     do (unless (hash issue)
          (save issue))
     do (vector-push-extend issue (trees issues-list)))
  (save-tree issues-list)
  (commit-tree issues-list)
  (ensure-directories-exist (uiop:pathname-directory-pathname *head-path*))
  (with-open-file (f *head-path*
                     :direction :output :if-exists :overwrite
                     :if-does-not-exist :create)
    (let ((buf (uiop:strcat (commit-hash issues-list) #\Newline)))
      (write-sequence buf f))))

(defmethod initialize-instance :after ((tree issues-list-tree) &key)
  "Read all the existing issues and put them in the 'issues' slot"
  (when (probe-file *head-path*)
    (loop for issue in (load-tree (uiop:read-file-line *head-path*))
       ;; Every entry in the top-level tree is a tree,
       ;; so we can just push them.
       do (vector-push-extend issue (issues tree)))))

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
    (vector-push-extend issue (issues issues-list))
    (save issues-list)))
