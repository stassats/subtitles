;;; -*- Mode: Lisp -*-

(defpackage #:subtitles
  (:use #:cl)
  (:export #:subtitles
           #:frame
           #:read-subtitles
           #:write-subtitles
           #:external-format
           #:register-type
           #:load-subtitle
           #:save-subtitle
           #:encode-time
           #:decode-time
           #:contents
           #:text
           #:start-time
           #:end-time
           #:*default-encoding*))

(defpackage #:subtitles.subrip
  (:use #:cl #:subtitles))

(defpackage #:subtitles.microdvd
  (:use #:cl #:subtitles))
