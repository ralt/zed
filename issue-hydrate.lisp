(in-package #:zed)

(defgeneric hydrate (object)
  (:documentation "Hydrates an object already fed a hash."))

(defmethod hydrate ((issue issue-tree))
  (dolist (item (load-tree (hash (git-tree issue))))
    (hydrate-git item)
    (cond ((string= (filename item) "title") (setf (title issue) (content item)))
          ((string= (filename item) "status") (setf
                                               (status issue)
                                               (string-downcase
                                                (intern (content item) *package*))))
          ((string= (filename item) "head")
           (let ((message-tree (make-instance 'issue-message-tree
                                              :git-tree item)))
             (hydrate message-tree)
             (setf (head issue) message-tree))))))

(defmethod hydrate ((message issue-message-tree))
  ;; Make sure to reuse the hydrate-git'd object
  (loop for item across (blobs (git-tree message))
     do (cond ((string= (filename item) "author") (setf (author message)
                                                        (content item)))
              ((string= (filename item) "date") (setf (date message)
                                                      (content item)))
              ((string= (filename item) "content") (setf (content message)
                                                         (content item)))))
  ;; The only tree is "children"
  nil)
