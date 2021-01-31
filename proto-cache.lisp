(defpackage #:proto-cache
  (:use #:cl)
  (:export #:register-publisher
           #:register-subscriber
           #:update-publisher-any
           #:save-state-to-file
           #:load-state-from-file
           #:main)
  (:local-nicknames
   (#:thread #:ace.core.thread)
   (#:defun #:ace.core.defun)
   (#:hook #:ace.core.hook)
   (#:etc #:ace.core.etc)
   (#:flag #:ace.flag)
   (#:google #:cl-protobufs.google.protobuf)
   (#:psd #:cl-protobufs.pub-sub-details)))

(in-package #:proto-cache)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Flag definitions

(flag:define flag::*load-file* ""
  "Specifies the file from which to load the PROTO-CACHE on start up."
  :type string)

(flag:define flag::*save-file* ""
  "Specifies the file to save PROTO-CACHE to on shutdown"
  :type string)

(flag:define flag::*new-subscriber* ""
  "URL for a new subscriber, just for testing"
  :type string)

(flag:define flag::*help* nil
  "Whether to print help"
  :type boolean)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Global definitions

(defvar *cache* (psd:make-pub-sub-details-cache))
(defvar *mutex-for-pub-sub-details* (make-hash-table :test 'equal))
(defvar *cache-mutex* (thread:make-frmutex))

(defun:defun* make-pub-sub-details (username password)
  "Make the pub-sub-details struct with a given password."
  (declare (defun:self (string string) psd:pub-sub-details))
  (psd:make-pub-sub-details :username username
                            :password password
                            :current-message (google:make-any)))

(defmethod (setf psd:current-message) :around (new-value (psd psd:pub-sub-details))
  "Set the new current-message field on a `pub-sub-details` class.
   Send the new google:any message to any subscriber."
  (let ((ps-mutex (gethash (psd:username psd) *mutex-for-pub-sub-details*)))
    (thread:with-frmutex-write (ps-mutex)
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
    (thread:with-frmutex-write (ps-mutex)
      (call-next-method))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The real pub-sub-functions.

(defun register-publisher (username password)
  "Register a publisher with the proto-cache."
  (let ((pub-sub-struct (make-pub-sub-details username password)))
    (thread:with-frmutex-write (*cache-mutex*)
      (unless (psd:pub-sub-cache-gethash username *cache*)
        (setf (psd:pub-sub-cache-gethash username *cache*)
              pub-sub-struct
              (gethash username *mutex-for-pub-sub-details*)
              (thread:make-frmutex))
        t))))

(defun register-subscriber (publisher address)
  "Register a new subscriber to a publisher."
  (etc:clet ((ps-struct
              (thread:with-frmutex-read (*cache-mutex*)
                (psd:pub-sub-cache-gethash publisher *cache*)))
             (ps-mutex
              (gethash publisher *mutex-for-pub-sub-details*)))
    (thread:with-frmutex-write (ps-mutex)
      (push address (psd:subscriber-list ps-struct)))))

(defun update-publisher-any (username password any)
  "Updates the google:any message for a publisher
   with a specified username and password.
   The actual subscriber calls happen in a separate thread
   but 'T is returned to the user to indicate the any
   was truly updated."
  (etc:clet ((ps-class
              (thread:with-frmutex-read (*cache-mutex*)
                (psd:pub-sub-cache-gethash username *cache*)))
             (correct-password (string= (psd:password ps-class)
                                        password)))
    (declare (ignore correct-password))
    (thread:make-thread
     (lambda (ps-class)
       (setf (psd:current-message ps-class) any))
     :arguments (list ps-class))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main for the executable.

(defun main ()
  (register-publisher "pika" "chu")
  (register-subscriber "pika" flag::*new-subscriber*)
  (update-publisher-any
   "pika" "chu"
   (google:make-any :type-url "a"))
  (sleep 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save load functions.

(defun save-state-to-file (&key (filename "/tmp/proto-cache.txt"))
  "Save the current state of the proto cache to *cache* global
   to FILENAME as a serialized protocol buffer message."
  (thread:with-frmutex-read (*cache-mutex*)
    (with-open-file (stream filename :direction :output
                                     :element-type '(unsigned-byte 8)
                                     :if-exists :supersede)
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
                   (thread:make-frmutex)))
    (thread:with-frmutex-write (*cache-mutex*)
      (setf *mutex-for-pub-sub-details* new-mutex-for-pub-sub-details
            *cache* new-cache))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load/Exit  Hooks.

(defmethod hook::at-restart parse-command-line ()
  "Parse the command line flags."
  (flag:parse-command-line)
  (when flag::*help*
    (flag:print-help)))

(defmethod hook::at-restart load-proto-cache :after parse-command-line  ()
  "Load the command line specified file at startup."
  (when (string/= flag::*load-file* "")
    (load-state-from-file :filename flag::*load-file*)))

(defmethod hook::at-exit save-proto-cache ()
  "Save the command line specified file at exit."
  (when (string/= flag::*save-file* "")
    (save-state-to-file :filename flag::*save-file*)))
