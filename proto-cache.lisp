(defpackage #:proto-cache
  (:use #:cl)
  (:export #:get-from-cache
           #:set-in-cache)
  (:local-nicknames
   (#:act #:ace.core.thread)
   (#:acd #:ace.core.defun)
   (#:google #:cl-protobufs.google.protobuf)))

(in-package #:proto-cache)

(defvar *cache* (make-hash-table :test 'eq))
(defvar *cache-mutex* (act:make-frmutex))

(acd:defun* get-from-cache (key)
  "Get the any message from cache with KEY."
  (declare (acd:self (symbol) google:any))
  (act:with-frmutex-read (*cache-mutex*)
    (gethash key *cache*)))

(acd:defun* set-in-cache (key any)
  "Set the ANY message in cache with KEY."
  (declare (acd:self (symbol google:any) google:any))
  (act:with-frmutex-write (*cache-mutex*)
    (setf (gethash key *cache*) any)))
