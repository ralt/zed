(asdf:defsystem #:zed
  :description "Git extension"
  :author "Florian Margaine <florian@margaine.com>"
  :license "GPL"
  :serial t
  :depends-on (:cl-syslog)
  :components ((:file "package")
               (:file "fs")
               (:file "run")
               (:file "git")
               (:file "git-storage")
               (:file "issue")
               (:file "commands")
               (:file "zed")))
