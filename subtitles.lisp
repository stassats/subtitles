;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:subtitles)

(defvar *default-encoding* :utf-8)

(defclass subtitles ()
  ((contents :initform nil
             :accessor contents
             :initarg :contents)))

(defclass frame ()
  ((text :initform nil
         :accessor text
         :initarg :text)
   (start-time :initform 0
               :accessor start-time
               :initarg :start-time)
   (end-time :initform 0
             :accessor end-time
             :initarg :end-time)))

(defgeneric read-subtitles (type stream))
(defgeneric write-subtitles (type subtitles stream))
(defgeneric external-format (type &optional encoding))

(defmethod external-format (type &optional (encoding *default-encoding*))
  (flex:make-external-format encoding))

(defvar *type-mapping* ())

(defun register-type (type function)
  (let ((acons (assoc type *type-mapping*)))
    (if acons
        (setf (cdr acons) function)
        (push (cons type function) *type-mapping*))))

(defun file-type (file-name)
  (loop for (type . function) in *type-mapping*
        when (funcall function file-name) return type))
;;;

(defun load-subtitle (file-name &optional (type (file-type file-name)))
  (unless type
    (error "Couldn't determine type of ~a." file-name))
  (with-open-file (stream file-name :element-type '(unsigned-byte 8))
    (let ((flexi (flex:make-flexi-stream stream :external-format
                                         (external-format type))))
      (read-subtitles type flexi))))

(defun save-subtitle (file-name subtitle &optional (type (file-type file-name)))
  (unless type
    (error "Couldn't determine type of ~a." file-name))
  (with-open-file (stream file-name :element-type '(unsigned-byte 8)
                          :direction :output)
    (let ((flexi (flex:make-flexi-stream stream :external-format
                                         (external-format type))))
      (write-subtitles type subtitle flexi))))

;;;

(defun encode-time (hours minutes seconds milliseconds)
  (+ milliseconds
     (* 1000 (+ seconds
                (* 60 (+ minutes
                         (* 60 hours)))))))

(defun decode-time (time)
  (flet ((remainder (divisor)
           (let (remainder)
             (setf (values time remainder) (truncate time divisor))
             remainder)))
    (let ((ms (remainder 1000))
          (s  (remainder 60))
          (m  (remainder 60))
          (h  time))
      (values h m s ms))))
