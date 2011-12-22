(in-package #:place-utils)

(defmacro %d (&rest defs)
  (declare (ignore defs)))

(%d * (* *)
    + (+ *)
    - (- *)
    / (/ *)
    /= (/= *)
    1+ 1-
    < (< *)
    <= (<= *)
    = (= *)
    > (> *)
    >= (>= *)
    abs
    (acons 3)
    acos acosh
    (adjoin 2)
    adjust-array
    adjustable-array-p
    allocate-instance
    alpha-char-p
    alphanumericp
    (macro and) (macro (and *))
    append (append *)
    (apply 2) (apply *)
    apropos-list
    aref ((aref *) 2) ; caution
    arithmetic-error-operands
    arithmetic-error-operation
    array-dimension (array-dimension *)
    array-dimensions
    array-displacement
    array-element-type
    array-has-fill-pointer-p
    array-in-bounds-p
    array-rank
    array-row-major-index
    array-total-size
    arrayp
    ash (ash *)
    asin asinh
    (assoc 2) (assoc-if 2) (assoc-if-not 2)
    ((assoc *) 1) ((assoc-if *) 1) ((assoc-if-not *) 1)
    atan ((atan *) 2) atanh
    atom
    bit ((bit *) 2) ; caution
    )

(macrolet ((d (&rest function-names)
	     `(%d ,name
		  ((,name *) 2))))
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

(%d bit-vector-p
    (boole 2) (boole *))

(define-modify-macro* boolean (&place generalized-boolean) ; caution
  (lambda (generalized-boolean)
    (if generalized-boolean t nil)))

(%d both-case-p
    boundp
    broadcast-stream-streams
    butlast
    byte (byte *)
    byte-position
    byte-size

    car cdr caar cadr cdar cddr
    caaar caadr cadar caddr cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr

    first second third fourth fifth
    sixth seventh eighth ninth tenth

    (macro case) (macro ccase) (macro ecase)
    (macro typecase) (macro ctypecase) (macro etypecase))

(macrolet ((d (&rest function-names)
	     `(%d ,name
		  ((,name *) 2))))
  (d ceiling fceiling
     floor ffloor
     round fround
     truncate ftruncate))

(%d cell-error-name
    char (char *)
    char-code
    code-char
    char-int
    char-name
    char-downcase
    char-upcase

    char-equal (char-equal *)
    char-greaterp (char-greaterp *)
    char-lessp (char-lessp *)
    char-not-equal (char-not-equal *)
    char-not-greaterp (char-not-greaterp *)
    char-not-lessp (char-not-lessp *)
    char/= (char/= *)
    char< (char< *)
    char<= (char<= *)
    char= (char= *)
    char> (char> *)
    char>= (char>= *)
    character
    characterp
    cis
    class-name
    class-of
    close
    coerce)

(define-modify-macro* compile (must-be-nil &place lambda-expression) ; caution
  (lambda (must-be-nil lambda-expression)
    (when must-be-nil
      (error "The first argument to MODIFY COMPILE must be NIL."))
    (unless (typep lambda-expression '(cons (eql lambda)))
      (error "COMPILEF can only compile lambda expressions."))
    (compile nil lambda-expression)))

(%d compile-file
    compile-file-pathname
    compiled-function-p
    compiler-macro-function
    complement
    complex ((complex *) 2)
    complexp
    compute-applicable-methods (compute-applicable-methods *)
    compute-restarts
    (concatenate 2) (concatenate *)
    concatenated-stream-streams
    conjugate
    cons (cons *)
    consp
    constantly
    constantp

    copy-alist
    copy-list
    copy-pprint-dispatch
    copy-readtable
    copy-seq
    copy-structure
    copy-symbol
    copy-tree

    cos cosh

    (count 2) (count-if 2) (count-if-not 2)

    decode-float
    decode-universal-time

    (delete 2) (delete-duplicates 2) (delete-if 2) (delete-if-not 2)

    delete-package
    denominator
    (deposit-field 3)
    (macro destructuring-bind 2)
    digit-char
    digit-char-p
    directory
    directory-namestring
    documentation
    (dpb 3)
    echo-stream-input-stream
    echo-stream-output-stream
    elt (elt *)
    endp
    enough-namestring ((enough-namestring *) 2)
    ensure-directories-exist ; supplied for 2nd return value
    ensure-generic-function
    eq (eq *)
    eql (eql *)
    equal (equal *)
    equalp (equalp *)
    evenp
    (every 2) (every *)
    exp
    expt (expt *)
    fboundp
    fdefinition
    file-author
    file-error-pathname
    file-length
    file-namestring
    file-position
    (file-string-length 2)
    file-write-date
    fill-pointer
    (find 2) (find-if 2) (find-if-not 2) ; caution
    ((find *) 1) ((find-if *) 1) ((find-if-not *) 1)
    find-all-symbols
    find-class
    find-method
    find-package
    find-restart
    find-symbol
    float
    float-digits
    float-precision
    float-radix
    float-sign ((float-sign *) 2)
    floatp
    (funcall 2) (funcall *)
    function-keywords
    function-lambda-expression
    functionp
    gcd (gcd *)
    gensym
    get ((get *) 2)
    get-macro-character ((get-macro-character *) 2)
    get-output-stream-string
    get-properties (get-properties *)
    get-setf-expansion
    getf ((getf *) 2)
    (gethash 2) ((gethash *) 1)
    graphic-char-p
    (macro handler-case)
    hash-table-count
    hash-table-p
    hash-table-rehash-size
    hash-table-rehash-threshold
    hash-table-size
    hash-table-test
    host-namestring
    identity
    imagpart
    initialize-instance
    input-stream-p
    integer-decode-float
    integer-length
    integerp
    interactive-stream-p
    intern ((intern *) 2)
    intersection ((intersection *) 2)
    invoke-restart
    invoke-restart-interactively
    isqrt
    keywordp
    last ((last *) 2)
    lcm (lcm *)
    (ldb 2)
    (ldb-test 2)
    ldiff ((ldiff *) 2)
    length
    list (list *)
    list-length
    listen
    listp
    load
    load-logical-pathname-translations
    log ((log *) 2)
    logand (logand *)
    logandc1 (logandc1 *)
    logandc2 (logandc2 *)
    (logbitp 2) ((logbitp *) 1) ; caution
    logcount
    logeqv (logeqv *)
    logical-pathname
    logical-pathname-translations
    logior (logior *)
    lognand (lognand *)
    lognor (lognor *)
    lognot
    logorc1
    logorc2
    logtest (logtest *)
    logxor (logxor *)
    lower-case-p
    macro-function
    macroexpand
    macroexpand-1
    make-array
    make-broadcast-stream (make-broadcast-stream *)
    make-concatenated-stream (make-concatenated-stream *)
    make-condition
    make-echo-stream (make-echo-stream *)
    make-instance
    make-list
    make-load-form
    make-load-form-saving-slots
    make-package
    make-random-state
    (make-sequence 2) ((make-sequence *) 1) ; caution
    make-string
    make-string-input-stream
    make-symbol
    make-synonym-stream
    make-two-way-stream (make-two-way-stream *)
    (map 3) (map *)
    (map-into 3) (map-into *)
    (mapc 2) (mapc *) ; caution, todo
    (mapcar 2) (mapcar *) (mapcan 2) (mapcan *)
    mapl ; caution, todo
    (maplist 2) (maplist *) (mapcon 2) (mapcon *)
    maphash ; caution, todo
    (maskfield 2)
    max (max *)
    (member 2) (member-if 2) (member-if-not 2) ; caution
    ((member *) 1) ((member-if *) 1) ((member-if-not *) 1)
    (merge 2) ((merge *) 3)
    merge-pathnames
    method-qualifiers
    min (min *)
    minusp
    mismatch ((mismatch *) 2)
    mod (mod *)
    multiple-value-call ; caution, todo
    )
