;;; -*- Mode: Lisp -*-

(defpackage #:subtitles
  (:use #:cl)
  (:export #:subtitles
           #:fragment
           #:fragment-by-time
           #:fragment-by-frame
           #:read-subtitles
           #:write-subtitles
           #:external-format
           #:register-type
           #:load-subtitles
           #:save-subtitles
           #:encode-time
           #:decode-time
           #:contents
           #:text
           #:start
           #:end
           #:*default-encoding*
           #:convert-frame-rate
           #:+23.976+
           #:+29.970+
           #:+25+))

(defpackage #:subtitles.subrip
  (:use #:cl #:subtitles))

(defpackage #:subtitles.microdvd
  (:use #:cl #:subtitles))
