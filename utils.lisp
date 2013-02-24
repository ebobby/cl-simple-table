;;; Simple table data structure.
;;; Copyright (c) 2013, Francisco Soto All rights reserved (see COPYING file for details).

(in-package :cl-simple-table)

(defun split-string (separator str)
  "Splits a string using the given separator, returns a list with the substrings."
  (declare (type character separator)
           (type string str))
  (loop
     with len = (length str)
     for fr = 0 then (1+ in)
     while (<= fr len)
     for in = (or (position separator str :test #'char= :start fr) len)
     collect (subseq str fr in)))
