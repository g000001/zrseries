(in-package :series)

#+symbolics(setf (gethash 'defun zwei:*lisp-indentation-offset-hash-table*)
                 '(2 1))
#+Symbolics
(setf (get 'defun 'zwei:definition-function-spec-parser)
      (get 'cl:defun 'zwei:definition-function-spec-parser))


#+symbolics(setf (gethash 'multiple-value-bind
                          zwei:*lisp-indentation-offset-hash-table*)
                 '(1 3 2 1))

#+symbolics(setf (gethash 'let zwei:*lisp-indentation-offset-hash-table*)
                 '(1 1))


#+symbolics(setf (gethash 'let* zwei:*lisp-indentation-offset-hash-table*)
                 '(1 1))

#+symbolics(setf (gethash 'iterate zwei:*lisp-indentation-offset-hash-table*)
                 '(1 1))

#+symbolics
(setf (gethash 'mapping zwei:*lisp-indentation-offset-hash-table*)
      '(1 1))

#+symbolics
(setf (gethash 'producing zwei:*lisp-indentation-offset-hash-table*)
      '(2 1))

#+symbolics
(cl:defun compute-series-macform-2 (name arglist doc body-code trigger 
					   local-p disc-expr opt-expr
				    unopt-expansion)
  #+:symbolics (declare (zl:arglist ,@(copy-list arglist))))

#+symbolics
(defmacro fragl (&rest stuff)
  #+symbolics (declare (scl:arglist args rets aux alt prolog body epilog wraprs))
  )

#+symbolics
(defmacro *fragl (&rest stuff)
  #+symbolics (declare (scl:arglist args rets aux alt prolog body epilog wraprs))
  )
