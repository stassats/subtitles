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

(defun read-frame-number (stream)
  (assert (char= (read-char stream) #\{))
  (read-integer stream #\}))

(defun read-text (stream)
  (read-line stream))

(defun read-frame (stream)
  (let ((frame (make-instance 'frame)))
    (with-slots (text start-time end-time) frame
      (setf start-time (read-frame-number stream)
            end-time (read-frame-number stream)
            text (read-text stream))
      frame)))

(defun write-frame-number (frame stream)
  (format stream "{~a}{~a}"
          (start-time frame)
          (end-time frame)))

(defun write-frame (frame stream)
  (write-frame-number frame stream)
  (write-line (text frame) stream))

(defmethod read-subtitles ((type (eql 'microdvd)) stream)
  (make-instance 'subtitles :contents
                 (loop while (listen stream)
                       collect (read-frame stream))))

(defmethod write-subtitles ((type (eql 'microdvd)) subtitles stream)
  (loop for frame in (contents subtitles)
        do (write-frame frame stream)))

(defmethod external-format ((type (eql 'microdvd))
                            &optional (encoding *default-encoding*))
  (flex:make-external-format encoding :eol-style :crlf))

(register-type 'microdvd
               (lambda (file-name)
                 (equalp (pathname-type file-name) "sub")))
