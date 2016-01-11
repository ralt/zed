(in-package #:zed)

(defgeneric hydrate (object)
  (:documentation "Hydrates an object already fed a hash."))

(defmethod hydrate ((issue issue-tree))
  (dolist (item (load-tree (hash issue)))
    (hydrate-git item)
    (cond ((string= (filename item) "title") (setf (title issue) (content item)))
          ((string= (filename item) "status") (setf
                                               (status issue)
                                               (intern (content item) *package*)))
          ((string= (filename item) "head")
           (let ((message-tree (make-instance 'issue-message-tree
                                              :git-tree item)))
             (hydrate message-tree)
             (setf (head issue) message-tree))))))

(defmethod hydrate ((message issue-message-tree)))
