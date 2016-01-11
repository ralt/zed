(in-package #:zed)

(defgeneric hydrate (object)
  (:documentation "Hydrates an object already fed a hash."))

(defmethod hydrate ((issue issue-tree)))
