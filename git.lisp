(in-package #:zed)

(defun git-config (value)
  (let ((ret (run "git config ~A" value)))
    (unless (string= ret "")
      ret)))

(defvar *default-editor* "vi")

(defun git-editor ()
  "Copy of const char *git_editor(void)"
  (let* ((editor (uiop:getenv "GIT_EDITOR"))
         (terminal (uiop:getenv "TERM"))
         (terminal-is-dumb (or (string= terminal "")
                               (string= terminal "dumb")))
         (editor-program (git-config "code.editor")))


    (when (and (null editor)
               (editor-program))
      (setf editor editor-program))

    (when (and (null editor)
               (not terminal-is-dumb))
      (setf editor (uiop:getenv "VISUAL")))

    (when (null editor)
      (setf editor (uiop:getenv "EDITOR")))

    (when (and (null editor)
               terminal-is-dumb)
      (return-from git-editor nil))

    (when (null editor)
      (setf editor *default-editor*))

    editor))

(defun launch-editor (path)
  (let ((editor (git-editor)))
    (if editor
        (launch-git-editor editor path)
        (error "Terminal is dumb, but EDITOR unset"))))

(defun launch-git-editor (editor path)
  (unless (find ":" editor)
    (handler-case
        (uiop:run-program (format nil "~A ~A" editor path) :pty t)
      (uiop:subprocess-error (err)
        (l :err "launch-git-editor: ~A" err)
        (error "There was a problem with editor ~A" editor))))
  (uiop:read-file-string path))

(defun git-root (path)
  (uiop:strcat (run "git rev-parse --show-toplevel") "/" path))

(defun git-author-name ()
  (or (uiop:getenv "GIT_AUTHOR_NAME")
      (git-config "user.name")
      (slot-value (sb-posix:getpwuid (sb-posix:geteuid)) 'sb-posix::name)))

(defun git-author-email ()
  (or (uiop:getenv "GIT_AUTHOR_EMAIL")
      (git-config "user.email")
      (let ((name (slot-value (sb-posix:getpwuid (sb-posix:geteuid))
                              'sb-posix::name)))
        (uiop:strcat name "@" name))))
