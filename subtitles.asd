;;; -*- Mode: Lisp -*-

(asdf:defsystem subtitles
  :serial t
  :depends-on (flexi-streams)
  :components ((:file "packages")
               (:file "subtitles")
               (:file "subrip")
               (:file "microdvd")))
