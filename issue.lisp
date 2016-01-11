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
    (save issues-list)))

(defun find-issue (list issue-hash)
  (find-if (lambda (issue)
             (string= (hash issue) issue-hash))
           (issues list)))

(defun issue-add-child (issue message-tree message)
  )
