(defpackage #:proto-cache
  (:use #:cl)
  (:export #:register-publisher
           #:register-subscriber
           #:update-publisher-any
           #:save-state-to-file
           #:load-state-from-file)
  (:local-nicknames
   (#:act #:ace.core.thread)
   (#:acd #:ace.core.defun)
   (#:ace #:ace.core.etc)
   (#:google #:cl-protobufs.google.protobuf)
   (#:psd #:cl-protobufs.pub-sub-details)))

(in-package #:proto-cache)

(defvar *cache* (psd:make-pub-sub-details-cache))
(defvar *mutex-for-pub-sub-details* (make-hash-table :test 'equal))
(defvar *cache-mutex* (act:make-frmutex))
;; (cl-protobufs.implementation::make-serializer psd:pub-sub-details-cache)

(acd:defun* make-pub-sub-details (username password)
  "Make the pub-sub-details struct with a given password."
  (declare (acd:self (string string) psd:pub-sub-details))
  (psd:make-pub-sub-details :username username
                            :password password
                            :current-message (google:make-any)))

(defmethod (setf psd:current-message) :around (new-value (psd psd:pub-sub-details))
  "Set the new current-message field on a `pub-sub-details` class.
   Send the new google:any message to any subscriber."
  (let ((ps-mutex (gethash (psd:username psd) *mutex-for-pub-sub-details*)))
    (act:with-frmutex-write (ps-mutex)
      (call-next-method))))

(defmethod (setf psd:current-message) :after (new-value (psd psd:pub-sub-details))
  (let ((subscriber-list (psd:subscriber-list psd)))
    (dolist (subscriber subscriber-list)
      (unwind-protect
           (drakma:http-request
            subscriber
            :content-type "application/octet-stream"
            :content (cl-protobufs:serialize-to-bytes new-value))
        (print subscriber)))))

(defmethod (setf psd:subscriber-list) :around (new-value (psd psd:pub-sub-details))
  "Setf function for the subscriber list.
   Locks the pub-sub-details frmutex before writing."
  (let ((ps-mutex (gethash (psd:username psd) *mutex-for-pub-sub-details*)))
    (act:with-frmutex-write (ps-mutex)
      (call-next-method))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The real pub-sub-functions.

(defun register-publisher (username password)
  "Register a publisher with the proto-cache."
  (let ((pub-sub-struct (make-pub-sub-details username password)))
    (act:with-frmutex-write (*cache-mutex*)
      (unless (psd:pub-sub-cache-gethash username *cache*)
        (setf (psd:pub-sub-cache-gethash username *cache*)
              pub-sub-struct
              (gethash username *mutex-for-pub-sub-details*)
              (act:make-frmutex))
        t))))

(defun register-subscriber (publisher address)
  "Register a new subscriber to a publisher."
  (ace:clet ((ps-struct
              (act:with-frmutex-read (*cache-mutex*)
                (psd:pub-sub-cache-gethash publisher *cache*)))
             (ps-mutex
              (gethash publisher *mutex-for-pub-sub-details*)))
    (act:with-frmutex-write (ps-mutex)
      (push address (psd:subscriber-list ps-struct)))))

(defun update-publisher-any (username password any)
  "Updates the google:any message for a publisher
   with a specified username and password.
   The actual subscriber calls happen in a separate thread
   but 'T is returned to the user to indicate the any
   was truly updated."
  (ace:clet ((ps-class
              (act:with-frmutex-read (*cache-mutex*)
                (psd:pub-sub-cache-gethash username *cache*)))
             (correct-password (string= (psd:password ps-class)
                                        password)))
    (declare (ignore correct-password))
    (act:make-thread
     (lambda (ps-class)
       (setf (psd:current-message ps-class) any))
     :arguments (list ps-class))
    t))

(defun save-state-to-file (&key (filename "/tmp/proto-cache.txt"))
  "Save the current state of the proto cache to *cache* global
   to FILENAME as a serialized protocol buffer message."
  (act:with-frmutex-read (*cache-mutex*)
    (with-open-file (stream filename :direction :output
                                     :element-type '(unsigned-byte 8))
      (cl-protobufs:serialize-to-stream stream *cache*))))

(defun load-state-from-file (&key (filename "/tmp/proto-cache.txt"))
  "Load the saved *cache* globals from FILENAME. Also creates
   all of the fr-mutexes that should be in *mutex-for-pub-sub-details*."
  (let ((new-cache
          (with-open-file (stream filename :element-type '(unsigned-byte 8))
            (cl-protobufs:deserialize-from-stream
             'psd:pub-sub-details-cache :stream stream)))
        (new-mutex-for-pub-sub-details (make-hash-table :test 'equal)))
    (loop for key being the hash-keys of (psd:pub-sub-cache new-cache)
          do
             (setf (gethash key new-mutex-for-pub-sub-details)
                   (act:make-frmutex)))
    (act:with-frmutex-write (*cache-mutex*)
      (setf *mutex-for-pub-sub-details* new-mutex-for-pub-sub-details
            *cache* new-cache))))
