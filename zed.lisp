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

(defcommand create (title)
  "git zed create <TITLE>
Create an issue."
  (let ((zed-msg-file (git-root ".git/ZED_MSG")))
    (touch zed-msg-file)
    (let ((msg (launch-editor zed-msg-file)))
      (when (string= msg "")
        (error "no comment found"))
      (issue-create title msg
                    (git-author-name) (git-author-email)
                    (get-universal-time)))))

(defcommand reply (issue-short-hash &optional message-short-hash)
  "git zed reply <ISSUE HASH> [MESSAGE HASH]
Reply to a message."
  (let ((issue-hash (run "git rev-parse ~A" issue-short-hash)))
    (format t "~A" issue-hash)))

(defcommand pull (remote)
  "git zed pull <REMOTE>
Pull the list of issues."
  (run "git fetch ~A refs/zed/issues/head:refs/zed/issues/head" remote))

(defcommand push (remote)
  "git zed push <REMOTE>
Push the list of issues."
  (run "git push ~A refs/zed/issues/head:refs/zed/issues/head" remote))
