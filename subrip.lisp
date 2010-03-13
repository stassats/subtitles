;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:subtitles.subrip)

(defun parse-time (string &optional (shift 0))
  "01:36:54,873 => milliseconds"
  (let ((spec '((0 2  #\: #.(* 60 60 1000))
                (3 5  #\: #.(* 60 1000))
                (6 8  #\, 1000)
                (9 12 nil 1))))
    (loop for (start end separator ms) in spec
          sum (* ms
                 (parse-integer string
                                :start (+ start shift)
                                :end (+ end shift)))
          while separator
          do (assert (eql separator (char string (+ end shift)))))))

(defun read-time (stream)
  (let* ((string (read-line stream))
         (arrow-end (+ (search " --> " string)
                       (length " --> "))))
    (values (parse-time string)
            (parse-time string arrow-end))))

(defun read-text (stream)
  (with-output-to-string (result)
    (loop with prev-char = #\Newline
          for char = (read-char stream nil)
          until (or (null char)
                    (char= prev-char char #\Newline))
          do (write-char char result)
          (setf prev-char char))
    result))

(defun read-frame (stream)
  (assert (integerp (parse-integer (read-line stream))))
  (let ((frame (make-instance 'frame)))
    (with-slots (text start-time end-time) frame
      (setf (values start-time end-time) (read-time stream)
            text (read-text stream))
      frame)))

(defun write-time (ms stream)
  (multiple-value-bind (h m s ms) (decode-time ms)
    (let ((spec `((2 #\: ,h)
                  (2 #\: ,m)
                  (2 #\, ,s)
                  (3 nil ,ms))))
     (loop for (digits separtor value) in spec
           do (format stream "~v,,,'0@a" digits value)
           when separtor do (write-char separtor stream)))))

(defun write-timings (frame stream)
  (write-time (start-time frame) stream)
  (write-string " --> " stream)
  (write-time (end-time frame) stream)
  (terpri stream))

(defun write-frame (frame n stream)
  (format stream "~a~%" n)
  (write-timings frame stream)
  (write-line (text frame) stream)
  (terpri stream))

(defmethod read-subtitles ((type (eql 'subrip)) stream)
  (make-instance 'subtitles :contents
                 (loop while (listen stream)
                       collect (read-frame stream))))

(defmethod write-subtitles ((type (eql 'subrip)) subtitles stream)
  (loop for number from 1
        for frame in (contents subtitles)
        do (write-frame frame number stream)))

(defmethod external-format ((type (eql 'subrip))
                            &optional (encoding *default-encoding*))
  (flex:make-external-format encoding :eol-style :crlf))

(register-type 'subrip
               (lambda (file-name)
                 (equalp (pathname-type file-name) "srt")))
