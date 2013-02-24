;;;; Simple table data structure.
;;;; Copyright (c) 2013, Francisco Soto All rights reserved (see COPYING file for details).

(in-package :cl-simple-table)

(defun table-from-file (filename &key (separator #\tab))
  "Reads the tabular data file and returns the contents. Separator is TAB by default."
  (with-open-file (s filename :if-does-not-exist nil)
    (row-sequence->table
     (loop
        for line = (read-line s nil nil)
        until (null line)
        collect (sequence->row (split-string separator line))))))

(defun read-csv (filename)
  "Creates a table from a comma-separated values file."
  (table-from-file filename :separator #\,))

(defun read-tsv (filename)
  "Creates a table from a tab-separated values file."
  (table-from-file filename :separator #\tab))
