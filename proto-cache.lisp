(defpackage #:proto-cache
  (:use #:cl)
  (:export #:register-publisher
           #:register-subscriber
           #:update-publisher-any)
  (:local-nicknames
   (#:act #:ace.core.thread)
   (#:acd #:ace.core.defun)
   (#:ace #:ace.core.etc)
   (#:google #:cl-protobufs.google.protobuf)))

(in-package #:proto-cache)

(defvar *cache* (make-hash-table :test 'equal))
(defvar *cache-mutex* (act:make-frmutex))

;; This pub-sub-details class stuff should be in it's
;; own file.
(defclass pub-sub-details ()
  ((subscriber-list :type list
                    :initform nil
                    :reader subscriber-list)
   (current-any :type google:any
                :initform (google:make-any)
                :reader current-any)
   (mutex :type act:frmutex
          :initform (act:make-frmutex)
          :reader mutex
          :documentation "A mutex to protext the current-any slot.")
   (password :type string
             :reader password
             :initarg :password
             :documentation "The worst way to store a password ever...")))

(acd:defun* make-pub-sub-details (password)
  "Make the pub-sub-details struct with a given password."
  (declare (acd:self (string) pub-sub-details))
  (make-instance 'pub-sub-details :password password))

(defmethod (setf current-any) (new-value (psd pub-sub-details))
  "Set the new current-any field on a `pub-sub-details` class.
   Send the new google:any message to any subscriber."
  (let ((ps-mutex (mutex psd)))
    (act:with-frmutex-write (ps-mutex)
      (setf (slot-value psd 'current-any) new-value))))

(defmethod (setf current-any) :after (new-value (psd pub-sub-details))
  (let ((subscriber-list (subscriber-list psd)))
    (dolist (subscriber subscriber-list)
      (unwind-protect
           (drakma:http-request
            subscriber
            :content-type "application/octet-stream"
            :content (cl-protobufs:serialize-to-bytes new-value))
        (print subscriber)))))

(defmethod (setf subscriber-list) (new-value (psd pub-sub-details))
  "Setf function for the subscriber list.
   Locks the pub-sub-details frmutex before writing."
  (let ((ps-mutex (mutex psd)))
    (act:with-frmutex-write (ps-mutex)
      (setf (slot-value psd 'subscriber-list) new-value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The real pub-sub-functions.

(defun register-publisher (username password)
  "Register a publisher with the proto-cache."
  (let ((pub-sub-struct (make-pub-sub-details password)))
    (act:with-frmutex-write (*cache-mutex*)
      (unless (gethash username *cache*)
        (setf (gethash username *cache*) pub-sub-struct)))))

(defun register-subscriber (publisher address)
  "Register a new subscriber to a publisher."
  (ace:clet ((ps-struct
              (act:with-frmutex-read (*cache-mutex*)
                (gethash publisher *cache*)))
             (ps-mutex (mutex ps-struct)))
    (act:with-frmutex-write (ps-mutex)
      (push address (subscriber-list ps-struct)))))

(defun update-publisher-any (username password any)
  "Updates the google:any message for a publisher
   with a specified username and password.
   The actual subscriber calls happen in a separate thread
   but 'T is returned to the user to indicate the any
   was truly updated."
  (ace:clet ((ps-class
              (act:with-frmutex-read (*cache-mutex*)
                (gethash username *cache*)))
             (correct-password (string= (password ps-class)
                                        password)))
    (declare (ignore correct-password))
    (act:make-thread
     (lambda (ps-class)
       (setf (current-any ps-class) any))
     :arguments (list ps-class))
    t))
