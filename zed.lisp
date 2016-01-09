(in-package #:zed)

(defun main (args)
  (if (second args)
      (multiple-value-bind
            (list present-p)
          (gethash (second args) *commands*)
        (if present-p
            (handler-case
                (apply (getf list :lambda) (rest (rest args)))
              (sb-int:simple-program-error () (usage)))
            (usage)))
      (usage)))

(defun l (priority text &rest args)
  (syslog:log "zed" :local0 priority (apply #'format nil text args)))

(defun usage ()
  (format t "usage: git zed <COMMAND> [ARGS]~%")
  (maphash (lambda (_ value)
             (declare (ignore _))
             (format
              t "~%~A"
              (reduce (lambda (doc line)
                        (format nil "~A~A~%~A~A~A~%~%" #\Tab doc #\Tab #\Tab line))
                      (uiop:split-string
                       (getf value :docstring)
                       :separator #(#\Newline)))))
           *commands*))

(defvar *msg-file* (git-root ".git/ZED_MSG"))

(defmacro with-editor (vars &body body)
  (let ((msg-file (second vars))
        (msg (first vars)))
    `(progn
       (touch ,msg-file)
       (let ((,msg (launch-editor ,msg-file)))
         (when (string= ,msg "")
           (error "no comment found"))
         ,@body)
       (delete-file *msg-file*))))

(defcommand create (title)
  "git zed create <TITLE>
Create an issue."
  (with-editor (msg *msg-file*)
    (issue-create title msg
                  (git-author-name) (git-author-email)
                  (get-universal-time))))

(defcommand reply (issue-short-hash &optional message-short-hash)
  "git zed reply <ISSUE HASH> [MESSAGE HASH]
Reply to a message."
  (let* ((issue-hash (long-hash issue-short-hash))
         (issue-tree (make-instance 'issue-tree :hash issue-hash)))
    (with-editor (msg *msg-file*)
      (issue-reply issue-tree
                   (or message-short-hash
                       (head issue-tree))
                   msg
                   (git-author-name)
                   (git-author-email)
                   (get-universal-time)))))

(defcommand pull (remote)
  "git zed pull <REMOTE>
Pull the list of issues."
  (run "git fetch ~A refs/zed/issues/head:refs/zed/issues/head" remote))

(defcommand push (remote)
  "git zed push <REMOTE>
Push the list of issues."
  (run "git push ~A refs/zed/issues/head:refs/zed/issues/head" remote))
