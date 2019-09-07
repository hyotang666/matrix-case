; vim: ft=lisp et
(in-package :asdf)
(defsystem :matrix-case
  :version "0.0.0"
  :components ((:file "matrix-case")))

;; The form below is added by JINGOH.GENERATOR.
(defmethod component-depends-on((o test-op) (c (eql (find-system "matrix-case"))))
  (append (call-next-method)'((test-op "matrix-case.test"))))
