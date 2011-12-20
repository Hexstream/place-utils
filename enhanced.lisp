(in-package #:place-utils)

(defmacro define-modify-macro* (name lambda-list function &optional docstring)
  (declare (ignore name lambda-list function docstring))
  )

(defmacro modify (&rest expressions)
  (declare (ignore expressions)))

#+nil(defmacro define-modify-macro (name lambda-list function &optional docstring)
  (flet ((&place-tail (list skip-other-llks-p)
	   (member-if (lambda (thing)
			(and (symbolp thing)
			     (char= (char (symbol-name thing) 0)
				    #\&)))
		      list)))
    (let* ((&place-tail (&place-tail lambda-list))
	   (lambda-list (if &place-tail
			    (if (&place-tail &place-tail)
				(error "There"))
			    ))
	   (implicit-&place-p (or (not first-llk-tail)
				  (not (eq first-llk-tail &place))))
	   (place-var)
	   (macro-lambda-list))
      `(defmacro ,name ,macro-lambda-list
	 (multiple-value-bind (vars vals stores writer reader)
	     (get-setf-expansion ,place-var env)
	   )))))
