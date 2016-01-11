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

(defun issue-reply (issue-tree message-tree content author-name author-email date))
