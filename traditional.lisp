(in-package #:place-utils)

(define-modify-macro* * (&place number &rest numbers))
(define-modify-macro* (* *) (&rest numbers &place number))

(define-modify-macro* + (&place number &rest numbers))
(define-modify-macro* (+ *) (&rest numbers &place number))

(define-modify-macro* - (&place number &rest numbers))
(define-modify-macro* (- *) (&rest numbers &place number))

(define-modify-macro* / (&place numerator &rest denominators))
(define-modify-macro* (/ *) (&rest numbers &place number))

(define-modify-macro* /= (&place number &rest numbers))
(define-modify-macro* (/= *) (&rest numbers &place number))

(define-modify-macro* 1+ (&place number))
(define-modify-macro* 1- (&place number))

(define-modify-macro* < (&place number &rest numbers))
(define-modify-macro* (< *) (&rest numbers &place number))

(define-modify-macro* <= (&place number &rest numbers))
(define-modify-macro* (<= *) (&rest numbers &place number))

(define-modify-macro* = (&place number &rest numbers))
(define-modify-macro* (= *) (&rest numbers &place number))

(define-modify-macro* > (&place number &rest numbers))
(define-modify-macro* (> *) (&rest numbers &place number))

(define-modify-macro* >= (&place number &rest numbers))
(define-modify-macro* (>= *) (&rest numbers &place number))

(define-modify-macro* abs (&place number))

(define-modify-macro* acons (key datum &place alist)) ; caution

(define-modify-macro* acos (&place number))
(define-modify-macro* acosh (&place number))

;; add-method

(define-modify-macro* adjoin (item &place list &key key test)) ; FE to pushnew

(define-modify-macro* adjust-array (&place array new-dimensions
					   &key element-type
					   initial-element
					   initial-contents
					   fill-pointer
					   displaced-to
					   displaced-index-offset))

(define-modify-macro* adjustable-array-p (&place array))

(define-modify-macro* allocate-instance (&place class &rest initargs))

(define-modify-macro* alpha-char-p (&place character))

(define-modify-macro* alphanumericp (&place character))

(define-modify-macro* and (&place form &rest forms))
(define-modify-macro* (and *) (&rest forms &place form))

(define-modify-macro* append (&place list &rest lists))
(define-modify-macro* (append *) (&rest lists &place list))

(define-modify-macro* apply (function &place arg &rest args)) ; caution
(define-modify-macro* (apply *) (function &rest args &place arg))

(define-modify-macro* apropos-list (&place string &optional package))

(define-modify-macro* aref (&place array &rest subscripts))
(define-modify-macro* (aref *) (array &place first-subscript &rest subscripts))

(define-modify-macro* arithmetic-error-operands (&place condition))
(define-modify-macro* arithmetic-error-operation (&place condition))

(define-modify-macro* array-dimension (&place array axis-number))
(define-modify-macro* (array-dimension *) (array &place axis-number))

(define-modify-macro* array-dimensions (&place array))

(define-modify-macro* array-displacement (&place array))
(define-modify-macro* (array-displacement 2) (&place array))

(define-modify-macro* array-element-type (&place array))

(define-modify-macro* array-has-fill-pointer-p (&place array))

(define-modify-macro* array-in-bounds-p (&place array &rest subscripts))

(define-modify-macro* array-rank (&place array))

(define-modify-macro* array-row-major-index (&place array &rest subscripts))

(define-modify-macro* array-total-size (&place array))

(define-modify-macro* arrayp (&place object))

(define-modify-macro* ash (&place integer count))
(define-modify-macro* (ash *) (integer &place count))

(define-modify-macro* asin (&place number))
(define-modify-macro* asinh (&place number))

(define-modify-macro* assoc (item &place alist &key key test))
(define-modify-macro* assoc-if (predicate &place alist &key key))
(define-modify-macro* assoc-if-not (predicate &place alist &key key))

(define-modify-macro* atan (&place number1 &optional number2))
(define-modify-macro* (atan *) (number1 &place number2))
(define-modify-macro* atanh (&place number))

(define-modify-macro* atom (&place object))

(define-modify-macro* bit (&place bit-array &rest subscripts))
(define-modify-macro* (bit *) (bit-array &place first-subscript &rest subscripts))

(macrolet ((d (&rest function-names)
	     `(progn
		,@(mapcan
		   (lambda (name)
		     (list `(define-modify-macro* ,name
				(&place bit-array1 bit-array2
					&optional optarg))
			   `(define-modify-macro* (,name *)
				(bit-array1 &place bit-array2
					    &optional result-bit-array))))
		   function-names))))
  (d bit-and
     bit-andc1
     bit-andc2
     bit-eqv
     bit-ior
     bit-nand
     bit-nor
     bit-not
     bit-orc1
     bit-orc2
     bit-xor))

(define-modify-macro* bit-vector-p (&place object))

(define-modify-macro* boole (op &place integer-1 integer-2))
(define-modify-macro* (boole *) (op integer-1 &place integer-2))

(define-modify-macro* boolean (&place generalized-boolean) ; caution
  (lambda (generalized-boolean)
    (if generalized-boolean t nil)))

(define-modify-macro* both-case-p (&place character))

(define-modify-macro* boundp (&place symbol))

(define-modify-macro* broadcast-stream-streams (&place broadcast-stream))

(define-modify-macro* butlast (&place list &optional n))

(define-modify-macro* byte (&place size position))
(define-modify-macro* (byte *) (size &place position))

(define-modify-macro* byte-position (&place bytespec))
(define-modify-macro* byte-size (&place bytespec))

(macrolet ((d (&rest function-names)
	     `(progn
		,@(mapcar
		   (lambda (name)
		     `(define-modify-macro* ,name (&place list) ,name))
		   function-names))))
  (d caar
     cadr
     car
     cdar
     cddr
     cdr))

(define-modify-macro* case (&place keyform &body cases))
(define-modify-macro* ccase (&place keyplace &body cases))
(define-modify-macro* ecase (&place keyform &body cases))

(macrolet ((d (&rest function-names)
	     `(progn
		,@(mapcan
		   (lambda (name)
		     (list `(define-modify-macro ,name
				(&place number &optional divisor))
			   `(define-modify-macro (,name *)
				(number &place divisor))
			   `(define-modify-macro (,name 2)
				(&place number &optional divisor))
			   `(define-modify-macro (,name * 2)
				(number &place divisor))))
		   function-names))))
  (d ceiling
     fceiling
     ffloor
     floor
     fround
     ftruncate))

(define-modify-macro* cell-error-name (&place condition))

(define-modify-macro* char (&place string index))
(define-modify-macro* (char *) (string &place index))

(define-modify-macro* char-code (&place character))
(define-modify-macro* code-char (&place code))
(define-modify-macro* char-int (&place character))
(define-modify-macro* char-name (&place character))
(define-modify-macro* char-downcase (&place character))
(define-modify-macro* char-upcase (&place character))

(macrolet ((d (&rest function-names)
	     `(progn
		,@(mapcan
		   (lambda (name)
		     (list `(define-modify-macro* ,name
				(&place character &rest characters))
			   `(define-modify-macro* (,name *)
				(&rest characters &place character))))
		   function-names))))
  (d char-equal
     char-greaterp
     char-lessp
     char-not-equal
     char-not-greaterp
     char-not-lessp
     char/=
     char<
     char<=
     char=
     char>
     char>=))

(define-modify-macro* character (&place character-designator))
(define-modify-macro* characterp (&place object))

(define-modify-macro* cis (&place radians))

(define-modify-macro* class-name (&place class))
(define-modify-macro* class-of (&place class))

(define-modify-macro* close (&place stream &key abort))

(define-modify-macro* coerce (&place object result-type))

(define-modify-macro* compile (must-be-nil &place lambda-expression) ; caution
  (lambda (must-be-nil lambda-expression)
    (when must-be-nil
      (error "The first argument to MODIFY COMPILE must be NIL."))
    (unless (typep lambda-expression '(cons (eql lambda)))
      (error "COMPILEF can only compile lambda expressions."))
    (compile nil lambda-expression)))

(define-modify-macro* compile-file (&place input-file
					   &key output-file
					   verbose
					   print
					   external-format))

(define-modify-macro* compile-file-pathname (input-file
					     &key output-file
					     &allow-other-keys))

(define-modify-macro* compiled-function-p (&place object))

(define-modify-macro* compiler-macro-function (&place name &optional environment))

(define-modify-macro* complement (&place function))

(define-modify-macro* complex (&place realpart &optional imagpart))
(define-modify-macro* (complex *) (realpart &place imagpart))

(define-modify-macro* complexp (&place object))

(define-modify-macro* compute-applicable-methods (&place generic-function
							 function-arguments))
(define-modify-macro* (compute-applicable-methods *) (generic-function
						      &place function-arguments))

(define-modify-macro* compute-restarts (&place condition))

(define-modify-macro* concatenate (result-type &place sequence &rest sequences))
(define-modify-macro* (concatenate *) (result-type &rest sequences &place sequence))

(define-modify-macro* concatenated-stream-streams (&place concatenated-stream))

(define-modify-macro* conjugate (&place number))

(define-modify-macro* cons (&place object-1 object-2))
(define-modify-macro* (cons *) (object-1 &place object-2)) ; FE to push

(define-modify-macro* consp (&place object))

(define-modify-macro* constantly (&place place))

(define-modify-macro* constantp (&place form &optional environment))




;; old begin
(flet ((helper (value &rest other-values)
	 (labels ((recurse (previous-value next-values)
		    (if previous-value
			(if (endp next-values)
			    previous-value
			    (recurse (car next-values) (cdr next-values)))
			(return-from andf-helper nil))))
	   (recurse value other-values))))
  (define-modify-macro andf (&place place &rest other-values) #'helper)
  (define-modify-macro andf* (&rest preceding-values &place place) #'helper))



(macrolet ((d (&rest function-names)
	     `(progn
		,@(mapcar
		   (lambda (name)
		     `(define-modify-macro ,(intern (format nil "~A~A" name 'f))
			  () ,name))
		   function-names))))
  (d caar
     cadr
     car
     cdar
     cddr
     cdr))

(macrolet ((d (&rest function-names)
	     `(progn
		,@(mapcar
		   (lambda (name)
		     `(define-modify-macro ,(intern (format nil "~A~A" name 'f))
			  (&place number-place &optional (divisor 1)) ,name))
		   function-names))))
  (d ceiling
     fceiling
     ffloor
     floor
     fround
     ftruncate))

(define-modify-macro char-downcasef (&place character-place) char-downcase)

(define-modify-macro char-upcasef (&place character-place) char-upcase)

(define-modify-macro characterf (&place character-designator-place) character)

(define-modify-macro coercef (result-type) coerce)

(define-modify-macro compilef (must-be-nil &place lambda-expression)
  (lambda (must-be-nil lambda-expression)
    (when must-be-nil
      (error "The first argument to COMPILEF must be NIL."))
    (unless (typep lambda-expression '(cons (eql lambda)))
      (error "COMPILEF can only compile lambda expressions."))
    (compile nil lambda-expression)))

(define-modify-macro complementf (&place function-place) complement)

(define-modify-macro concatenatef (result-type
				   &place sequence-place
				   &rest other-sequences)
  concatenate)
(define-modify-macro concatenatef* (result-type
				    &rest preceding-sequences
				    &place sequence-place)
  concatenate)

(define-modify-macro constantlyf () constantly)

(define-modify-macro copy-alistf (&place alist-place) copy-alist)

(define-modify-macro copy-listf (&place list-place) copy-list)

(define-modify-macro copy-pprint-dispatchf (&place dispatch-table-place)
  copy-pprint-dispatch)

(define-modify-macro copy-readtablef (&place from-readtable-place to-readtable)
  copy-readtable)

(define-modify-macro copy-readtablef* (&place from-readtable to-readtable-place)
  copy-readtable)

(define-modify-macro copy-seqf (&place sequence-place) copy-seq)

(define-modify-macro copy-structuref (&place structure-place) copy-structure)

(define-modify-macro copy-symbolf (&place symbol-place &optional copy-properties)
  copy-symbol)

(define-modify-macro copy-treef (&place tree-place) copy-tree)

(define-modify-macro deletef (item &place sequence-place &rest rest
			      &key from-end test test-not start end count key)
  delete)

(define-modify-macro delete-duplicatesf (&place sequence-place &rest rest
					 &key from-end test test-not start end key)
  delete-duplicates)

(define-modify-macro delete-iff (test &place sequence-place &rest rest
				 &key from-end start end count key)
  delete-if)

(define-modify-macro delete-if-notf (test &place sequence-place &rest rest
				     &key from-end start end count key)
  delete-if-not)

(define-modify-macro deposit-fieldf (newbyte bytespec &place integer)
  deposit-field)

(define-modify-macro digit-charf (&place weight-place &optional radix)
  digit-char)

(define-modify-macro digit-char-pf (&place char-place &optional radix)
  digit-char-p)

(define-modify-macro directoryf (&place pathspec-place &rest keys &key)
  directory)

(define-modify-macro directory-namestringf (&place pathname-designator-place)
  directory-namestring)

(define-modify-macro dpbf (newbyte bytespec &place integer-place) dpb)

(define-modify-macro enough-namestringf (&place pathname-place &optional defaults)
  enough-namestring)

(define-modify-macro evalf (&place form-place) eval)

(define-modify-macro fdefinitionf (&place function-name-place)
  fdefinition)

(define-modify-macro fifthf (&place list-place) fifth)

(define-modify-macro find-classf (&place symbol-place &optional errorp environment)
  find-class)

(define-modify-macro find-packagef (&place string-designator-or-package)
  find-package)

(define-modify-macro find-restartf (&place symbol-or-restart &optional condition)
  find-restart)

(define-modify-macro firstf (&place list-place) first)

(define-modify-macro floatf (&place number-place &optional prototype))

(define-modify-macro fourthf (&place list-place) first)

(define-modify-macro funcallf (function &place arg-place &rest other-args) funcall)
(define-modify-macro funcallf* (function &rest preceding-args &place arg-place) funcall)

;(define-modify-macro gcdf ())
