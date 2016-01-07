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
  (format t "usage: git zed <COMMAND> [ARGS]~%"))

(defcommand create (title)
  "Create an issue."
  (let ((zed-msg-file (git-root ".git/ZED_MSG")))
    (touch zed-msg-file)
    (let ((msg (launch-editor zed-msg-file)))
      (when (string= msg "")
        (error "no comment found"))
      (issue-create title msg
                    (git-author-name) (git-author-email)
                    (get-universal-time)))))

(defcommand pull (remote)
  "Pull the list of issues."
  (run "git fetch ~A refs/zed/issues/head:refs/zed/issues/head" remote))

(defcommand push (remote)
  "Push the list of issues."
  (run "git push ~A refs/zed/issues/head:refs/zed/issues/head" remote))
