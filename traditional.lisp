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
    (macro or (macro (or *)))
    append (append *)
    nconc (nconc *)
    (apply 2) (apply *)
    apropos-list
    aref ((aref *) 2) ; caution
    svref (svref *)
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
    bit ((bit *) 2) sbit ((sbit *) 2); caution
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
    nbutlast
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
    char (char *) schar (schar *)
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
    (cons 2) ((cons *) 1)
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
    ensure-directories-exist	       ; supplied for 2nd return value
    ensure-generic-function
    eq (eq *)
    eql (eql *)
    equal (equal *)
    equalp (equalp *)
    evenp
    (every 2) (every *)
    (notany 2) (notany *)
    (notevery 2) (notevery *)
    (some 2) (some *)
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
    input-stream-p
    integer-decode-float
    integer-length
    integerp
    interactive-stream-p
    intern ((intern *) 2)
    intersection ((intersection *) 2)
    nintersection ((nintersection *) 2)
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
    (list* :last) ((list* *) 1)
    list-length
    listen
    listp
    load
    load-logical-pathname-translations
    log ((log *) 2)
    logand (logand *)
    logandc1 (logandc1 *)
    logandc2 (logandc2 *)
    (logbitp 2) ((logbitp *) 1)		; caution
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
    (mapc 2) (mapc *)			; caution, todo
    (mapcar 2) (mapcar *) (mapcan 2) (mapcan *)
    mapl				; caution, todo
    (maplist 2) (maplist *) (mapcon 2) (mapcon *)
    maphash				; caution, todo
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
    multiple-value-call			; caution, todo
    name-char
    namestring
    no-applicable-method
    no-next-method
    nreconc (nreconc *)
    nreverse
    nset-difference ((nset-difference *) 2)
    nset-exclusive-or ((nset-exclusive-or *) 2)
    nstring-capitalize
    nstring-downcase
    nstring-upcase
    (nsublis 2) ((nsublis *) 1)
    (nsubst 3) (nsubst-if 3) (nsubst-if-not 3)
    (nsubstitute 3) (nsubstitute-if 3) (nsubstitute-if-not 3)
    (nth 2) ((nth *) 1)
    (nthcdr 2) ((nthcdr *) 1)
    null
    numberp
    numerator
    nunion ((nunion *) 2)
    oddp
    open
    open-stream-p
    output-stream-p
    package-error-package
    package-name
    package-nicknames
    package-shadowing-symbols
    package-use-list
    package-used-by
    packagep
    (pairlis 3)
    parse-integer
    parse-namestring
    pathname
    pathname-device
    pathname-directory
    pathname-host
    pathname-match-p (pathname-match-p *)
    pathname-name
    pathname-type
    pathname-version
    pathnamep
    phase
    plusp
    (position 2) (position-if 2) (position-if-not 2)
    (pprint-dispatch 2) ((pprint-dispatch *) 1)
    (pprint-fill 2) (pprint-linear 2) (pprint-tabular 2)
    prin1 prin1-to-string
    princ princ-to-string
    print-not-readable-object
    probe-file
    (random 2) ((random *) 1)
    random-state-p
    (rassoc 2) ((rassoc *) 1) (rassoc-if 2) (rassoc-if-not 2)
    rational rationalize
    rationalp
    read
    read-byte
    read-char
    read-char-no-hang
    (read-delimited-list 2) ((read-delimited-list *) 1)
    read-from-string
    read-line
    read-preserving-whitespace
    read-sequence
    readtable-case
    readtablep
    realp
    realpart
    (reduce 2)
    rem (rem *)
    (remhash 2) ((remhash *) 1)
    (remove 2) ((remove *) 1) (remove-if 2) (remove-if-not 2)
    remove-duplicates
    rename-file (rename-file *)
    rename-package
    replace ((replace *) 2)
    rest
    restart-name
    revappend (revappend *)
    reverse
    row-major-aref (row-major-aref *)
    scale-float (scale-float *)
    search ((search *) 2)
    set
    set-difference ((set-difference *) 2)
    set-exclusive-or ((set-exclusive-or *) 2)
    signum
    simple-bit-vector-p
    simple-condition-format-arguments
    simple-condition-format-control
    simple-string-p
    simple-vector-p
    sin sinh
    slot-boundp (slot-boundp *)
    slot-exists-p (slot-exists-p *)
    slot-value (slot-value *)
    sort
    special-operator-p
    sqrt
    stable-sort
    standard-char-p
    stream-element-type
    stream-error-stream
    stream-external-format
    streamp
    string
    string-capitalize
    string-downcase
    string-equal ((string-equal *) 2)
    string-greaterp ((string-greaterp *) 2)
    (string-left-trim 2) ((string-left-trim *) 1)
    (string-right-trim 2) ((string-right-trim *) 1)
    (string-trim 2) ((string-trim *) 1)
    string-lessp ((string-lessp *) 2)
    string-not-equal ((string-not-equal *) 2)
    string-not-greaterp ((string-not-greaterp *) 2)
    string-not-lessp ((string-not-lessp *) 2)
    string-upcase
    string/= ((string/= *) 2)
    string< ((string< *) 2)
    string<= ((string<= *) 2)
    string= ((string= *) 2)
    string> ((string> *) 2)
    string>= ((string>= *) 2)
    stringp
    (sublis 2) ((sublis *) 1)
    subseq
    subsetp ((subsetp *) 2)
    (subst 3) (subst-if 3) (subst-if-not 3)
    (substitute 3) (substitute-if 3) (substitute-if-not 3)
    subtypep ((subtypep *) 2)
    sxhash
    symbol-function
    symbol-name
    symbol-package
    symbol-plist
    symbol-value
    symbolp
    synonym-stream-symbol
    (tailp 2) ((tailp *) 1)
    tan tanh
    ; the: todo
    translate-logical-pathname
    translate-pathname
    tree-equal ((tree-equal *) 2)
    truename
    two-way-stream-input-stream
    two-way-stream-output-stream
    type-error-datum
    type-error-expected-type
    type-of
    typep ((typep *) 2)
    unbound-slot-instance
    unintern ((unintern *) 2)
    union ((union *) 2)
    ;unless todo
    ;when todo
    upgraded-array-element-type
    upgraded-complex-part-type
    upper-case-p
    user-homedir-pathname
    vector (vector *)
    vector-pop
    (vector-push 2) ((vector-push *) 1)
    (vector-push-extend 2) ((vector-push-extend *) 1)
    vectorp
    wild-pathname-p
    write
    write-to-string
    zerop)
