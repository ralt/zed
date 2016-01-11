(in-package #:zed)

(defvar *head-path* (git-root ".git/refs/zed/issues/head"))

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
     do (unless (and (hash issue)
                     (null (updated issue)))
          (save issue))
     do (vector-push-extend issue (trees issues-list)))
  (save-tree issues-list)
  (commit-tree *head-path* issues-list))
