(in-package #:zed)

(defun git-config (value)
  (run "git config ~A" value))

(defvar *default-editor* "vi")

(defun git-editor ()
  "Copy of const char *git_editor(void)"
  (let* ((editor (uiop:getenv "GIT_EDITOR"))
         (terminal (uiop:getenv "TERM"))
         (terminal-is-dumb (or (string= terminal "")
                               (string= terminal "dumb")))
         (editor-program (git-config "code.editor")))


    (when (and (null editor)
               (not (string= editor-program "")))
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

(defun launch-editor (path env)
  (let ((editor (git-editor)))
    (if editor
        (launch-git-editor editor path env)
        (error "Terminal is dumb, but EDITOR unset"))))

(defun launch-git-editor (editor path env)
  (unless (find ":" editor)
    (handler-case
        (progn
          (uiop:run-program (format nil "~A ~A" editor path) :environment env)
          t)
      (uiop:subprocess-error () (error "There was a problem with editor ~A" editor))))
  (uiop:read-file-string path))
