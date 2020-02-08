(defpackage :matrix-case
  (:use :cl)
  (:export #:matrix-case
           #:matrix-ecase
           #:matrix-ccase
           #:matrix-typecase
           #:matrix-etypecase
           #:matrix-ctypecase))

(in-package :matrix-case)

(defmacro defmatrix (underlying)
  `(defmacro ,(intern (format nil "MATRIX-~A" underlying))
             ((&rest targets) &body clauses)
     (flet ((canonicalize (clauses)
              (let ((otherwise
                     (find 'otherwise clauses
                           :key #'car
                           :test #'eq
                           :from-end t)))
                (if otherwise
                    (values
                      (remove 'otherwise clauses ; Don't ever use DELETE!
                              :key #'car
                              :from-end t
                              :count 1)
                      (list (cons t (cdr otherwise))))
                    (values clauses nil)))))
       ;; trivial syntax check.
       (assert
        (loop :for (candidates) :in clauses
              :with length = (length targets)
              :always (if (eq 'otherwise candidates)
                          t
                          (= length (length candidates))))
        nil "TARGETS length does not match CANDIDATES length.~%~S~S" targets
        (mapcar #'car clauses))
       ;; body
       (car
         (multiple-value-call #'matrix
           ',underlying
           targets
           (canonicalize clauses))))))

(defun matrix (underlying targets clauses default)
  (if (null targets)
      (if (< 1 (length clauses))
          (error "Matrix impl bug:~%targets = ~S~%clauses = ~S" targets
                 clauses)
          (cdar clauses))
      (labels ((make-form (assoc)
                 (destructuring-bind
                     (type . branches)
                     assoc
                   (if branches
                       `(,type
                         ,@(matrix underlying (cdr targets) branches default))
                       (error "Impl bug: no branches~%assoc = ~S" assoc)))))
        (let ((var (gensym))
              (c
               (mapcar #'make-form
                       (canonicalize
                         (integrate-candidates clauses underlying)))))
          `((let ((,var ,(car targets)))
              (,underlying ,var ,@c
               ,@(when (and default (not (eq t (caar (last c)))))
                   default))))))))

(defun integrate-candidates (clauses underlying)
  ;; -> (candidate (branches . body)+)*
  (labels ((rec (clauses &optional acc)
             (if (endp clauses)
                 acc
                 (body (car clauses) (cdr clauses) acc)))
           (body (clause rest acc)
             (destructuring-bind
                 (candidates . body)
                 clause
               (if (find underlying '(typecase etypecase ctypecase) :test #'eq)
                   (rec rest
                        (integrate (car candidates) (cdr candidates) body acc))
                   (if (atom (car candidates))
                       (rec rest
                            (integrate (car candidates) (cdr candidates) body
                                       acc))
                       (rec rest
                            (reduce
                              (lambda (acc candidate)
                                (integrate candidate (cdr candidates) body
                                           acc))
                              (car candidates)
                              :initial-value acc))))))
           (integrate (candidate branches body acc)
             (let ((assoc (assoc candidate acc :test #'equal)))
               (when assoc
                 (setf (cdr assoc) (nconc (cdr assoc) `((,branches ,@body)))))
               (if assoc
                   acc
                   (nconc acc `((,candidate (,branches ,@body))))))))
    (rec clauses)))

(defun canonicalize (alist)
  (labels ((rec (alist &optional default acc)
             (if (endp alist)
                 (do-return default acc)
                 (body (car alist) (cdr alist) default acc)))
           (do-return (default acc)
             (assert (null (cdr default))) ; 1 element.
             (loop :for (key . rest) :in acc
                   :collect `(,key
                              ,@(delete-duplicates (append rest (cdar default))
                                                   :key #'car
                                                   :test #'equal
                                                   :from-end t)) :into result
                   :finally (return (nreconc result default))))
           (body (clause rest default acc)
             (if (eq t (car clause))
                 (rec rest (cons clause default) acc)
                 (rec rest default (cons clause acc)))))
    (rec alist)))

(macrolet ((defs (&rest symbols)
             `(progn ,@(mapcar (lambda (s) `(defmatrix ,s)) symbols))))
  (defs typecase etypecase ctypecase case ecase ccase))