;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: ASDF -*-

(defsystem "cl-simple-table"
  :version "1.1.0"
  :name "cl-simple-table"
  :author "Francisco Soto <ebobby@ebobby.org>"
  :maintainer  "Steve Nunez <steve@symbolics.tech>"
  :license "BSD"
  :description "Simple in-memory tabular data structure and utility methods."
  :depends-on ("alexandria")
  :components ((:file "packages")
               (:file "utils" :depends-on ("packages"))
               (:file "table" :depends-on ("packages"))
               (:file "query" :depends-on ("packages" "table"))
               (:file "importers" :depends-on ("packages" "table" "utils"))))
