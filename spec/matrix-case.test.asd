; vim: ft=lisp et
(in-package :asdf)
(defsystem :matrix-case.test :depends-on (:jingoh "matrix-case") :components
 ((:file "matrix-case")) :perform
 (test-op (o c) (symbol-call :jingoh :examine :matrix-case)))
