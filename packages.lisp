;;; Simple table data structure.
;;; Copyright (c) 2013, Francisco Soto All rights reserved (see COPYING file for details).

(in-package :cl-user)

(defpackage :cl-simple-table
  (:nicknames :simple-table)
  (:use :cl)
  (:export :make-table
           :make-row
           :add-to-table
           :num-rows
           :num-cols
           :get-row
           :add-to-row
           :get-row-column
           :num-col
           :rectangular-table-p
           :sequence->row
           :row-sequence->table
           :with-rows
           :select
           :distinct
           :top
           :order-by
           :where
           :where-filter
           :where-or
           :where-and
           :read-csv
           :read-tsv
           :table-from-file))
