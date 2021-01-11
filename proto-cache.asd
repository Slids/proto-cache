(defsystem :proto-cache
  :author "Jonathan Godbout"
  :version "0.0.1"
  :licence "MIT-style"
  :description      "A simple cache utility for protos."
  :long-description "A simple cache utility for protos."
  :depends-on (:ace.core :cl-protobufs :md5)
  :components
  ((:module "src"
    :serial t
    :pathname ""
    :components ((:file "proto-cache")))))
