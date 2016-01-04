(asdf:defsystem #:zed
  :description "Git extension"
  :author "Florian Margaine <florian@margaine.com>"
  :license "GPL"
  :serial t
  :components ((:file "package")
               (:file "run")
               (:file "git")
               (:file "commands")
               (:file "zed")))
