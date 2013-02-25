;;;; Simple table data structure.
;;;; Copyright (c) 2013, Francisco Soto All rights reserved (see COPYING file for details).

(in-package :cl-simple-table)

(defun select (table &rest columns)
  "Selects the given columns from the table and returns them as a new table."
  (let ((result (make-table)))
    (with-rows (table row result)
      (let ((new-row (make-row)))
        (mapc (lambda (col)
                (add-to-row (get-row-column col row) new-row))
              columns)
        (add-to-table new-row result)))))

(defun distinct (table column)
  "Returns the unique elements from the given column in the given table as a new table."
  (let ((added (make-hash-table :test #'equal))
        (result (make-table)))
    (with-rows (table row result)
      (let ((value (get-row-column column row)))
        (unless (gethash value added)
          (let ((new-row (make-row)))
            (setf (gethash value added) t)
            (add-to-row value new-row)
            (add-to-table new-row result)))))))

(defun top (table n)
  "Returns a new table with the top n rows from the given table."
  (let ((how-many (min n (num-rows table))))
    (subseq table 0 how-many)))

(defun order-by (table col op)
  "Returns a new table sorted by the value in the given column and table using op."
  (sort table op :key (lambda (row) (get-row-column col row))))

(defun where (table filter)
  "Filters the result of the table using the given operator."
  (remove-if-not filter
                 table))

(defun where-filter (op column value)
  "Returns a filter applicable for where, it calls op to compare the given value and the
   value stored in column for every row. Besides calling op the filter returned will also
   check the type of the values are the same before being compared."
  (let ((value-type (type-of value)))
    (lambda (row)
      (let ((val (get-row-column column row)))
        (and (typep val value-type)
             (funcall op value (get-row-column column row)))))))

(defun where-or (&rest filters)
  "Given a list of filters created by where-filter this returns true if any of them is true."
  (lambda (row) (some (lambda (filter)(funcall filter row))
                      filters)))

(defun where-and (&rest filters)
  "Given a list of filters created by where-filter this returns true if all of them are true."
  (lambda (row) (every (lambda (filter) (funcall filter row))
                       filters)))
