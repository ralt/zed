(in-package #:zed)

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

(defun issue-reply (issue-hash message-tree content author-name author-email date)
  (let* ((message (make-instance 'issue-message-tree
                                 :author (format nil "~A <~A>"
                                                 author-name author-email)
                                 :date date
                                 :content content))
         (issues-list (make-instance 'issues-list-tree))
         (issue (find-issue issues-list issue-hash)))
    (hydrate issue)
    (issue-add-child issue message-tree message)
    ;; This is necessary to set the 'updated' flag of an issue.
    (updated issue)
    (save issues-list)))

(defun find-issue (list issue-hash)
  (find-if (lambda (issue)
             (string= (hash (git-tree issue)) issue-hash))
           (issues list)))

(defun issue-add-child (issue message-tree message)
  (unless (children (head issue))
    (setf (children (head issue)) (make-instance 'issue-messages-list-tree)))
  (vector-push-extend
   message
   (messages (children (if message-tree
                           (find-issue-in-children issue message-tree)
                           (head issue))))))

(defun find-issue-in-children (issue child-hash)
  (let ((child? (find-if (lambda (child)
                           (string= (hash (git-tree child)) child-hash))
                         (children issue))))
    (if child? child?
        (loop for child across (children issue)
           with sub-child = (find-issue-in-children issue (hash (git-tree child)))
           when sub-child
           return sub-child))))

(defun updated (issue)
  (setf (updated issue) t)
  (when (children issue)
    (loop for child across (children issue)
       do (updated child))))
