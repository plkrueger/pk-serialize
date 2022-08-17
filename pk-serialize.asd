(asdf:defsystem #:pk-serialize
  :depends-on (#:closer-mop)
  :name "pk-serialize"
  :author "plkrueger <plkrueger@comcast.net>"
  :maintainer "plkrueger"
  :licence "MIT"
  :description "Serialization of Common Lisp data structures"
  :components ((:file "serialize")))