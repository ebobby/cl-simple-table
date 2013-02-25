;;;; Simple table data structure.
;;;; Copyright (c) 2013, Francisco Soto All rights reserved (see COPYING file for details).

(in-package :cl-simple-table)

(defun table-from-file (filename &key (separator #\tab) parse-elements)
  "Reads the tabular data file and returns the contents. Separator is TAB by default.
   If parse-elements is other than NIL elements from the table will be READ into Lisp objects,
   otherwise only strings will be created."
  (let ((filter (if parse-elements
                    (lambda (ln) (mapcar (lambda (el) (read-from-string el nil))
                                         (split-string separator ln)))
                    (lambda (ln) (split-string separator ln)))))
    (with-open-file (s filename :if-does-not-exist nil)
      (row-sequence->table
       (loop
          for line = (read-line s nil nil)
          until (null line)
          collect (sequence->row (funcall filter line)))))))

(defun read-csv (filename &optional parse-elements)
  "Creates a table from a comma-separated values file."
  (table-from-file filename :separator #\, :parse-elements parse-elements))

(defun read-tsv (filename &optional parse-elements)
  "Creates a table from a tab-separated values file."
  (table-from-file filename :separator #\tab :parse-elements parse-elements))
