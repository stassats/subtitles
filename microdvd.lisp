;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:subtitles.microdvd)

(defun read-integer (stream stop-character)
  (loop for char = (read-char stream)
        for digit = (digit-char-p char)
        while digit
        for result = digit then (+ (* result 10) digit)
        finally (progn
                  (assert (char= stop-character char))
                  (return result))))

(defun read-fragment-number (stream)
  (assert (char= (read-char stream) #\{))
  (read-integer stream #\}))

(defun read-text (stream)
  (read-line stream))

(defun read-fragment (stream)
  (let ((fragment (make-instance 'fragment-by-frame)))
    (with-slots (text start end) fragment
      (setf start (read-fragment-number stream)
            end (read-fragment-number stream)
            text (read-text stream))
      fragment)))

(defun write-fragment-number (fragment stream)
  (format stream "{~a}{~a}"
          (start fragment)
          (end fragment)))

(defun write-fragment (fragment stream)
  (write-fragment-number fragment stream)
  (write-line (text fragment) stream))

(defmethod read-subtitles ((type (eql 'microdvd)) stream)
  (make-instance 'subtitles :contents
                 (loop while (listen stream)
                       collect (read-fragment stream))))

(defmethod write-subtitles ((type (eql 'microdvd)) subtitles stream)
  (loop for fragment in (contents subtitles)
        do (write-fragment fragment stream)))

(defmethod external-format ((type (eql 'microdvd))
                            &optional (encoding *default-encoding*))
  (flex:make-external-format encoding :eol-style :crlf))

(register-type 'microdvd
               (lambda (file-name)
                 (equalp (pathname-type file-name) "sub")))
