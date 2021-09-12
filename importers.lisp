;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-SIMPLE-TABLE -*-
(in-package :cl-simple-table)

(defparameter *default-external-format* :default
  "External format used for opening files")

(defun white-space? (c)
  (member c '(#\newline #\tab #\space #\return)))

(defvar *quote* #\" "Default quote character")
(defvar *separator* #\, "Default separator character")
(defvar *escape* #\" "Default escape character")

;;; I/O from various sources
(defun %in-stream (stream-or-string)
  (typecase stream-or-string
    (string (make-string-input-stream stream-or-string))
    (stream stream-or-string)
    (pathname (values (open stream-or-string :external-format *default-external-format*)
                      T))))

(defmacro with-csv-input-stream ((name inp) &body body)
  (alexandria:with-unique-names (opened?)
    `(multiple-value-bind (,name ,opened?) (%in-stream ,inp)
      (flet ((body () ,@body))
        (unwind-protect (body)
          (when (and ,name ,opened?)
            (close ,name)))))))


;;; Readers
(defun table-from-stream (stream-or-string &key (separator *separator*) parse-elements skip-first-row?)
  "Reads the tabular data file from a string, stream or filepath and returns the contents. Separator is TAB by default.
   If parse-elements is other than NIL elements from the table will be READ into Lisp objects,
   otherwise only strings will be created."
  (let* ((filter (if parse-elements
                     (lambda (ln) (mapcar (lambda (el) (read-from-string el nil))
				     (split-string separator ln)))
		     (lambda (ln) (split-string separator ln)))))
    (with-csv-input-stream (s stream-or-string)
      (row-sequence->table
       (loop
          for line = (read-line s nil nil)
          until (null line)
	  unless skip-first-row?
	    collect (sequence->row (funcall filter line))
	  do (setf skip-first-row? nil))))))

(defun read-csv (filename &key parse-elements skip-first-row?)
  "Creates a table from a comma-separated values file."
  (table-from-stream filename :separator #\, :parse-elements parse-elements :skip-first-row? skip-first-row?))

(defun read-tsv (filename &key parse-elements skip-first-row?)
  "Creates a table from a tab-separated values file."
  (table-from-stream filename :separator #\tab :parse-elements parse-elements :skip-first-row? skip-first-row?))
