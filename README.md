# MATRIX-CASE 0.0.0
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
(matrix-typecase(:a :b)
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
CCL/1.11.5
ECL/16.1.3

### Known issues
#### with SBCL.
3 tests fails due to SBCL internal bug (?).

## Installation
TODO
