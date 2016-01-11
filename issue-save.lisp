(in-package #:zed)

(defgeneric save (object)
  (:documentation "Saves an issue."))

(defmethod save ((message issue-message-tree))
  (let ((message-tree (git-tree message)))
    (add-blob message-tree (make-instance 'git-blob
                                          :filename "author"
                                          :content (author message)))
    (add-blob message-tree (make-instance 'git-blob
                                          :filename "date"
                                          :content (write-to-string (date message))))
    (add-blob message-tree (make-instance 'git-blob
                                          :filename "content"
                                          :content (content message))))
  (when (> (length (children message)) 0)
    (let ((children-tree (make-instance 'git-tree :filename "children")))
      (loop for child across (children message)
         do (add-tree children-tree child))
      (add-tree (git-tree message) children-tree)))
  (save-tree (git-tree message)))

(defmethod save ((issue issue-tree))
  (let ((issue-tree (git-tree issue)))
    (add-blob issue-tree (make-instance 'git-blob
                                        :filename "title"
                                        :content (title issue)))
    (add-blob issue-tree (make-instance 'git-blob
                                        :filename "status"
                                        :content (string-downcase
                                                  (symbol-name (status issue)))))
    (setf (slot-value (head issue) 'filename) "head")
    (save (head issue))
    (add-tree issue-tree (git-tree (head issue)))
    (save-tree issue-tree)))

(defmethod save ((issues-list issues-list-tree))
  (let ((issues-list-tree (git-tree issues-list)))
    (loop for issue across (issues issues-list)
       do (unless (and (hash issue)
                       (null (updated issue)))
            (save issue))
       do (add-tree issues-list-tree issue))
    (save-tree issues-list-tree)
    (commit-tree *head-path* issues-list-tree)))
