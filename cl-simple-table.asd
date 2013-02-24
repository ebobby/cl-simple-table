;;;; Simple table data structure.
;;;; Copyright (c) 2013, Francisco Soto All rights reserved (see COPYING file for details).

(in-package :cl-user)

(defpackage :cl-simple-table-asd
  (:use :cl :asdf))

(in-package :cl-simple-table-asd)

(asdf:defsystem #:cl-simple-table
  :version "0.0.1"
  :name "cl-simple-table"
  :author "Francisco Soto <ebobby@ebobby.org>"
  :license "BSD"
  :description "Simple in-memory tabular data structure and utility methods."
  :components ((:file "packages")
               (:file "utils" :depends-on ("packages"))
               (:file "table" :depends-on ("packages"))
               (:file "query" :depends-on ("packages" "table"))
               (:file "importers" :depends-on ("packages" "table" "utils"))))
