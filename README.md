# MATRIX-CASE 0.0.6
## What is this?
Control flow macros which writing nested CASE easily.

### Current lisp world
No such feature.

### Issues
Hard to read/write.

Some pattern match library may provides same feature.
But it is too much strong.

### Proposal

## Usage

```Lisp
(matrix-typecase (:a :b)
  ((null null) :never)
  ((keyword keyword) :yes))
=> :YES
```

## From developer

### Product's goal
Integrated to popular utilities libraries. (e.g. alexandria)

### License
Public domain

### Developed with
SBCL

### Tested with
* SBCL/2.0.2
* CCL/1.12
* ECL/20.4.24
* CLISP/2.49

## Installation
TODO
