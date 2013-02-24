;;;; Simple table data structure.
;;;; Copyright (c) 2013, Francisco Soto All rights reserved (see COPYING file for details).

(in-package :cl-simple-table)

(deftype row ()
  "Table row type."
  `(vector t *))

(deftype table ()
  "Table type."
  `(vector row *))

(defun make-table ()
  "Creates a table."
  (make-array 1 :element-type 'row :fill-pointer 0 :adjustable t))

(defun make-row ()
  "Create a row."
  (make-array 1 :fill-pointer 0 :adjustable t))

(defun add-to-table (row table)
  "Appends a row to the table."
  (vector-push-extend row table)
  table)

(defun add-to-row (value row)
  "Append a column to row and set it to the given value."
  (vector-push-extend value row)
  row)

(defun num-rows (table)
  "Returns the number of rows in the table."
  (length table))

(defun num-cols (row)
  "Returns the number of elements in this row."
  (length row))

(defun get-row (index table)
  "Returns the row in the given index inside the table."
  (elt table index))

(defun get-row-column (column row)
  "Gets the value in the given column inside row."
  (elt row column))

(defun rectangular-table-p (table)
  "Returns true if all the rows in the table have the same number of elements."
  (or (= (num-rows table) 0)
      (let ((cols (num-cols (get-row 0 table))))
        (every (lambda (row)
                 (eql (num-cols row) cols))
               table))))

(defun sequence->row (elements)
  "Converts a sequence of elements into a table row."
  (coerce elements 'row))

(defun row-sequence->table (rows)
  "Converts a sequence of rows into a table."
  (coerce rows 'table))

(defmacro with-rows ((table row-var &optional return-expression) &body body)
  "Iterates the rows in the given table, row-var is the current row, returning return-expression."
  (let ((iterator (gensym)))
    `(dotimes (,iterator (num-rows ,table) ,return-expression)
       (let ((,row-var (get-row ,iterator ,table)))
         ,@body))))
