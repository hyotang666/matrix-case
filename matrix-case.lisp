(defpackage :matrix-case(:use :cl)
  (:export
    #:matrix-case
    #:matrix-ecase
    #:matrix-ccase
    #:matrix-typecase
    #:matrix-etypecase
    #:matrix-ctypecase
    ))
(in-package :matrix-case)

(defmacro defmatrix(underlying)
  `(DEFMACRO ,(intern(format nil "MATRIX-~A"underlying))
     ((&REST TARGETS)&BODY CLAUSES)
     (FLET((CANONICALIZE(CLAUSES)
	     (LET((OTHERWISE(FIND 'OTHERWISE CLAUSES
				  :KEY #'CAR :TEST #'EQ :FROM-END T)))
	       (IF OTHERWISE
		 (VALUES (DELETE 'OTHERWISE CLAUSES
				 :KEY #'CAR :FROM-END T :COUNT 1)
			 (LIST (CONS T (CDR OTHERWISE))))
		 (VALUES CLAUSES NIL))))
	   )
       ;; trivial syntax check.
       (ASSERT (LOOP :FOR (CANDIDATES) :IN CLAUSES
		     :WITH LENGTH = (LENGTH TARGETS)
		     :ALWAYS (IF(EQ 'OTHERWISE CANDIDATES)
			       t
			       (= LENGTH (LENGTH CANDIDATES))))
	       ()
	       "TARGETS length does not match CANDIDATES length.~%~S~S"
	       TARGETS (MAPCAR #'CAR CLAUSES))
       ;; body
       (CAR(MULTIPLE-VALUE-CALL #'MATRIX ',underlying TARGETS (CANONICALIZE CLAUSES))))))

(defun matrix(underlying targets clauses default)
  (if(null targets)
    (if(< 1 (length clauses))
      (error "Matrix impl bug:~%targets = ~S~%clauses = ~S"targets clauses)
      (cdar clauses))
    (labels((make-form(assoc)
	      (destructuring-bind(type . branches)assoc
		(if branches
		  `(,type ,@(matrix underlying (cdr targets)
				    branches
				    default))
		  (error "Impl bug: no branches~%assoc = ~S"assoc))))
	    )
      (let((var (gensym))
	   (c(mapcar #'make-form (canonicalize(integrate-candidates clauses)))))
	`((LET((,var ,(car targets)))
	    (,underlying ,var
	      ,@c
	      ,@(when(and default
			  (not(eq t (caar(last c)))))
		  default))))))))

(defun integrate-candidates(clauses)
  (loop :for (candidates . body) :in clauses
	:for assoc = (assoc (car candidates) acc :test #'equal)
	:if assoc
	:do (push `(,(cdr candidates),@body)
		  (cdr (assoc (car candidates) acc :test #'equal)))
	:else
	:collect (list (car candidates)
		       `(,(cdr candidates),@body))
	:into acc
	:finally (return (mapcar (lambda(assoc)
				   (rplacd assoc (nreverse(cdr assoc))))
				 acc))))

(defun canonicalize(alist)
  (labels((rec(alist &optional default acc)
	    (if(endp alist)
	      (do-return default acc)
	      (body(car alist)(cdr alist)default acc)))
	  (do-return(default acc)
	    (assert(null(cdr default))) ; 1 element.
	    (loop :for (key . rest) :in acc
		  :collect `(,key ,@(delete-duplicates (append rest (cdar default))
						       :key #'car
						       :test #'equal
						       :from-end T))
		  :into result
		  :finally (return (nreconc result default))))
	  (body(clause rest default acc)
	    (if(eq t (car clause))
	      (rec rest (cons clause default)acc)
	      (rec rest default (cons clause acc))))
	  )
    (rec alist)))

(macrolet((defs(&rest symbols)
	    `(PROGN ,@(mapcar (lambda(s)`(DEFMATRIX ,s))
			      symbols))))
  (defs typecase etypecase ctypecase case ecase ccase))
