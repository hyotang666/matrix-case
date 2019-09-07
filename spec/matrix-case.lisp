(defpackage :matrix-case.spec (:use :cl :jingoh :matrix-case))
(in-package :matrix-case.spec)
(setup :matrix-case)

(common-requirements-about (MATRIX-TYPECASE MATRIX-ETYPECASE MATRIX-CTYPECASE)
			   :as op)
;;;; [Macro] MATRIX-TYPECASE MATRIX-ETYPECASE MATRIX-CTYPECASE

#| Description: Like CL:TYPECASE, but represents nested typecases.|#
#?(let((a 0))
    (op(a 'key)
      ((symbol integer):never)
      ((integer symbol):yes!)
      (otherwise :no!)))
=> :YES!

;; Only one cluase's body will be evaluated.
#?(op(0 'sym 1)
    ((symbol symbol symbol)(princ :sss))
    ((symbol symbol integer)(princ :ssi))
    ((symbol integer symbol)(princ :sis))
    ((symbol integer integer)(princ :sii))
    ((integer symbol integer)(princ :yes))
    ((integer symbol symbol)(princ :iss))
    ((integer integer integer)(princ :iii))
    ((integer integer symbol)(princ :iis))
    (otherwise (princ :otherwise)))
:outputs "YES"

;; Of course, we can use compound type specifier.
#?(op(0)
    (((and integer (eql 1))) :no)
    (((member 0 2 4 6 8)) :yes))
=> :YES

;; T as wildcard
#?(op("hoge" 0)
    ((t symbol):no)
    ((t fixnum):yes)
    ((string string):no)
    )
=> :yes

;; NOTE! wildcard is treated as last resort.
#?(op("1" "2")
    ((t string) :no)
    ((string string):yes))
=> :yes

#?(op("1" "2")
    ((string t) :no)
    ((string string):yes))
=> :yes

;; NOTE! Each clause is tested left to right order.
#?(op(:a :b)
    ((null null):no)
    ((atom atom):yes))
=> :yes

#+syntax
(MATRIX-TYPECASE targets &body clauses) ; => result

;;; Arguments and Values:

#| targets := (target*)
   target := one lisp form
|#
;; every TARGET forms are evaluated only once.
#?(op((princ 0)(princ 1))
    ((symbol symbol):no)
    ((integer symbol):no)
    ((symbol integer):no)
    (otherwise :no))
:outputs "01"

#| clauses := ((typespecifier+) &body body)
   typespecifier := see hyperspec
   body := implicit PROGN
|#

#| result := T |#

#| Affected By: none |#

#| Side-Effects: none |#

#| Notes: |#
;; Empty TARGETS is valid but, in such case CLAUSES must empty.
;; Such form evaluated to be NIL by macro expansion.
#?(op()()) => NIL
#?(op()()) :expanded-to NIL
#?(op()) => NIL
#?(op()) :expanded-to NIL
#?(op) :signals ERROR

#| Exceptional-Situations: |#
;; when TARGETS length and each TYPE-SPECIFIERS length is different,
;; an error is signaled.
#?(op(0)
    ((symbol integer) :error))
:signals ERROR

#?(op(0 :hoge)
    ((integer) :error))
:signals ERROR

;; others
(requirements-about matrix-typecase)
;; Default return value is NIL.
#?(matrix-typecase(0)
    ((symbol) :no))
=> NIL

;; Expanded examples.
#?(matrix-typecase(0)
    ((integer) :yes)
    (otherwise :no))
:expanded-to (let((var 0))
	       (typecase var
		 (integer :yes)
		 (t :no)))

#?(matrix-typecase(0 'sym)
    ((symbol integer) :no)
    ((integer symbol) :yes)
    (otherwise :never))
:expanded-to (let((var1 0))
	       (typecase var1
		 (symbol (let((var2 'sym))
			   (typecase var2
			     (integer :no)
			     (t :never))))
		 (integer (let((var3 'sym))
			    (typecase var3
			      (symbol :yes)
			      (t :never))))
		 (t :never)))

(common-requirements-about (matrix-etypecase matrix-ctypecase)
			   :as op)
;; When any clause satisfies, an ERROR is signaled.
#?(op(0)
    ((symbol) :never))
:signals ERROR

(common-requirements-about (MATRIX-CASE MATRIX-ECASE MATRIX-CCASE)
			   :as op)

;;;; [Macro] MATRIX-CASE MATRIX-ECASE MATRIX-CCASE 

#| Description: Like CL:CASE, but represents nested case.|#
#?(op(0 1 2)
    ((0 0 0):no)
    ((0 1 1):no)
    ((0 1 2):yes)
    (otherwise :never))
=> :YES
#?(matrix-case(0 1)
    (((0 2 4 6 8)(1 3 5 7 9)) :yes)
    (((0 2 4 6 8)(0 2 4 6 8)) :no)
    (((1 3 5 7 9)(0 2 4 6 8)) :no))
=> :YES

#+syntax
(MATRIX-CASE (&rest keyform) &body clause*) ; => result

;;; Arguments and Values:

#| keyform := a form; evaluated to produce a test-key.|#

#| clause := ((test-key*) body)
   test-key := an object produced by evaluating keyform.
   body := an implicit PROGN. |#

#| result := the values returned by the body in the matching clause. |#

#| Affected By: none |#

#| Side-Effects: none |#

#| Notes: |#

#| Exceptional-Situations: |#

