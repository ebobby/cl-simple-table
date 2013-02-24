cl-simple-table - A Common Lisp simple in-memory table.
===

*cl-simple-table* is a set of functions to create rows and tables of rows and run some queries in them. Also included are simple functions to create tables from tabular data files, such as _CSV_ (comma separated values) or _TSV_ (tab separated values). This makes this library very useful to work with these files.

Internally, tables are just arrays of arrays (not multidimensional, actual array of arrays) although simple wrappers functions are provided to keep the context inside the problem space, you care about tabular data, not arrays.

## Dependencies

None.

## Usage

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

## Final remarks

I hope this code is useful to you in any sense, either for learning, reading or maybe actual practical use, I will be very glad if you can even modify it to suit your needs. If you have suggestions please send them my way. Be sure to read *COPYING* file as well.
