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
  (loop for blob across (blobs (git-tree message))
     do (cond ((string= (filename blob) "author") (setf (author message)
                                                        (content blob)))
              ((string= (filename blob) "date") (setf (date message)
                                                      (content blob)))
              ((string= (filename blob) "content") (setf (content message)
                                                         (content blob)))))
  ;; The only tree is "children", and it's not always there
  (when (has-tree (git-tree message))
    ;; todo
    nil))
