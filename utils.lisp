;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-SIMPLE-TABLE -*-
(in-package :cl-simple-table)

(defun split-string (separator str)
  "Splits a string using the given separator, returns a list with the substrings."
  (declare (type character separator)
           (type string str))
  (loop
     with len = (length str)
     for start = 0 then (1+ end)
     while (<= start len)
     for end = (or (position separator str :test #'char= :start start)
		   len)
     collect (subseq str start end)))
