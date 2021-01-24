(defsystem :proto-cache
  :author "Jonathan Godbout"
  :version "0.0.1"
  :licence "MIT-style"
  :description      "A simple cache utility for protos."
  :long-description "A simple cache utility for protos."
  :defsystem-depends-on (:cl-protobufs)
  :depends-on (:ace.core :cl-protobufs :drakma)
  :components
  ((:module "src"
    :serial t
    :pathname ""
    :components
    ((:protobuf-source-file "pub-sub-details"
      :proto-pathname "pub-sub-details.proto"
      :proto-search-path ("../cl-protobufs/google/protobuf/"))
     (:file "proto-cache")))))
