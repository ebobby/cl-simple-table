cl-simple-table - A Common Lisp simple in-memory table.
===

*cl-simple-table* is a set of functions to create rows and tables of rows and run some queries in them. Also included are simple functions to create tables from tabular data files, such as _CSV_ (comma separated values) or _TSV_ (tab separated values). This makes this library very useful to work with these files.

Internally, tables are just arrays of arrays (not multidimensional, actual array of arrays) although simple wrappers functions are provided to keep the context inside the problem space, you care about tabular data, not arrays.

## Dependencies

None.

# Usage

## Table and row handling.

###     (make-table ())

Creates a new empty table and returns it.

Example:

```Lisp
CL-USER> (simple-table:make-table)
#()
```

###     (make-row ())

Creates a new empty table and returns it.

Example:

```Lisp
CL-USER> (simple-table:make-row)
#()
```

###     (add-to-table (row table))

Appends the row to the table and returns the table.

Example:

```Lisp
CL-USER> (simple-table:add-to-table
          (simple-table:add-to-row "Hello" (simple-table:make-row))
          (simple-table:make-table))
#(#("Hello"))
```

###     (add-to-row (value row))

Appends the value as a new column in the given row and returns the row.

Example:

```Lisp
CL-USER> (simple-table:add-to-row "Hello" (simple-table:make-row))
#("Hello")
CL-USER> (simple-table:add-to-row
          ", world!"
          (simple-table:add-to-row "Hello" (simple-table:make-row)))
#("Hello" ", world!")
```

###     (get-row (index table))

Gets the row at the given index from the table. The first row is index 0.

Example:

```Lisp
CL-USER> (simple-table:get-row 0 *data*)
#("Year" "Make" "Model")
CL-USER> (simple-table:get-row 1 *data*)
#("1997" "Ford" "E350")
```

###     (get-row-column (column row))

Gets the value in the given column from row. The first column is 0.

Example:

```Lisp
CL-USER> (simple-table:get-row-column 0 (simple-table:get-row 1 *data*))
"1997"
CL-USER> (simple-table:get-row-column 1 (simple-table:get-row 1 *data*))
"Ford"
```

###     (set-row-column (column value row))

Sets the value in the given column of row and returns the row. The first column is 0.

Example:

```Lisp
CL-USER> (simple-table:set-row-column
          2 "Mustang"
          (simple-table:set-row-column 0 "1967" (simple-table:get-row 1 *data*)))
#("1967" "Ford" "Mustang")
```

###     (num-rows (table))

Returns the number of rows in the given table.

Example:

```Lisp
CL-USER> (simple-table:num-rows (simple-table:make-table))
0
CL-USER> (simple-table:num-rows (simple-table:add-to-table
                                 (simple-table:make-row)
                                 (simple-table:make-table)))
1
```

###     (num-cols (row))

Returns the number of values in the given row.

Example:

```Lisp
CL-USER> (simple-table:num-cols (simple-table:make-row))
0
CL-USER> (simple-table:num-cols (simple-table:add-to-row
                                 "Hello, world!"
                                 (simple-table:make-row)))
1
```

###     (rectangular-table-p (table))

Returns true if all the rows in the table have the same number of elements.

Example:

```Lisp
CL-USER> (simple-table:rectangular-table-p (simple-table:make-table))
T
CL-USER> (simple-table:rectangular-table-p (simple-table:add-to-table
                                            (simple-table:make-row)
                                            (simple-table:make-table)))
T
CL-USER> (simple-table:rectangular-table-p (simple-table:add-to-table
                                            (simple-table:add-to-row
                                             "Oops!"
                                             (simple-table:make-row))
                                            (simple-table:add-to-table
                                             (simple-table:make-row)
                                             (simple-table:make-table))))
NIL
```

###     (sequence->row (elements))

Converts a sequence of elements into a table row.

Example:

```Lisp
CL-USER> (simple-table:sequence->row (list 1 2 3))
#(1 2 3)
CL-USER> (simple-table:sequence->row "Hello") ; Strings are sequence of characters!
#(#\H #\e #\l #\l #\o)
```

###     (row-sequence->table (rows))

Converts a sequence of rows into a table.

Example:

```Lisp
CL-USER> (simple-table:row-sequence->table (list
                                            (simple-table:sequence->row (list "Col1" "Col2"))
                                            (simple-table:sequence->row (list 1 2))
                                            (simple-table:sequence->row (list 2 3))))
#(#("Col1" "Col2") #(1 2) #(2 3))
```

###     (with-rows ((table row-var &optional return-expression) &body body)

*with-rows* is a macro to help you iterate the rows on a given _table_ and with a reference to each row in _row-var_, optionally returning _return-expression_.

Example:

```Lisp
CL-USER> (simple-table:with-rows (*data* row)
           (print row))

#("Year" "Make" "Model")
#("1967" "Ford" "Mustang")
#("2000" "Mercury" "Cougar")
NIL

CL-USER> (let ((new-table (simple-table:make-table)))
           (simple-table:with-rows (*data* row new-table)
             (simple-table:add-to-table (simple-table:add-to-row
                                         (simple-table:get-row-column 1 row)
                                         (simple-table:make-row))
                                        new-table)))
#(#("Make") #("Ford") #("Mercury"))
```

## Data importing.

###     (table-from-file (filename &key (separator #\tab) parse-elements))

Reads a tabular data file pointed by _filename_ with columns separated by _separator_, if _parse-elements_ is not NIL then *READ* will be applied to each element and parsed into Lisp objects, otherwise it creates only strings. It returns the table with the data read.

Example:

```Lisp
CL-USER> (simple-table:table-from-file #P"example.csv" :separator #\, :parse-elements t)
#(#(YEAR MAKE MODEL) #(1997 FORD E350) #(2000 MERCURY COUGAR))

CL-USER> (simple-table:table-from-file #P"example.csv" :separator #\,)
#(#("Year" "Make" "Model") #("1997" "Ford" "E350") #("2000" "Mercury" "Cougar"))
```

###     (read-csv (filename &optional parse-elements))

Reads a csv (comma separated values) file into a table, optionally parsing the elements into Lisp objects. This is a wrapper for *table-from-file*.

```Lisp
CL-USER> (simple-table:read-csv #P"example.csv" t)
#(#(YEAR MAKE MODEL) #(1997 FORD E350) #(2000 MERCURY COUGAR))
CL-USER> (simple-table:read-csv #P"example.csv")
#(#("Year" "Make" "Model") #("1997" "Ford" "E350") #("2000" "Mercury" "Cougar"))
```

###     (read-tsv (filename &optional parse-elements))

Reads a tsv (table separated values) file into a table, optionally parsing the elements into Lisp objects. This is a wrapper for *table-from-file*.

```Lisp
CL-USER> (simple-table:read-tsv #P"example.tsv" t)
#(#(YEAR MAKE MODEL) #(1997 FORD E350) #(2000 MERCURY COUGAR))
CL-USER> (simple-table:read-tsv #P"example.tsv")
#(#("Year" "Make" "Model") #("1997" "Ford" "E350") #("2000" "Mercury" "Cougar"))
```

## Final remarks

I hope this code is useful to you in any sense, either for learning, reading or maybe actual practical use, I will be very glad if you can even modify it to suit your needs. If you have suggestions please send them my way. Be sure to read *COPYING* file as well.
