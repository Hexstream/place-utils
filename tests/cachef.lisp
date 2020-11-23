(cl:in-package #:place-utils_tests)

(define-test "CACHEF")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CACHEF - test utilities

(defvar *result*)

(defun loudcar (cons &optional cachedp)
  (push (if cachedp :cachedp-read :place-read) *result*)
  (car cons))

(defun (setf loudcar) (newval cons &optional cachedp)
  (push (if cachedp :cachedp-written :place-written) *result*)
  (setf (car cons) newval))

(defun verify (expected)
  (is equal expected (nreverse *result*))
  (setf *result* '()))

(defun make-empty-adjustable-string ()
  (make-array 0 :element-type 'character :fill-pointer t :adjustable t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CACHEF - examples

(define-test "CACHEF-EXAMPLE-ICC"
  :parent "CACHEF"
  (flet ((perform (initial-value)
           (let ((cache initial-value)
                 (output (make-empty-adjustable-string)))
             (with-output-to-string (*standard-output* output)
               (incf (cachef nil cache 0 :test #'numberp) (princ (+ 5 2)))
               (list cache output)))))
    (is equal '(7 "7") (perform "cached-string"))
    (is equal '(27 "7") (perform 20))))

(define-test "CACHEF-EXAMPLE-OOCC"
  :parent "CACHEF"
  (let ((values (list :empty :placeholder))
        (output (make-empty-adjustable-string)))
    (flet ((fullp (marker) (ecase marker (:full t) (:empty nil))))
      (with-output-to-string (*standard-output* output)
        (cachef (first values) (second (prin1 values)) :computed-value
                :test #'fullp :new-cachedp :full)
        (is equal '((:full :computed-value) "(:EMPTY :PLACEHOLDER)")
            (list values output))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CACHEF - basic tests

(define-test "CACHEF-ICC-BASIC"
  :parent "CACHEF"
  (macrolet ((perform (place newval expected &rest cachef-args)
               (a:with-gensyms (result)
                 `(let ((,result (cachef nil ,place ,newval ,@cachef-args)))
                    (is equal ,expected ,result)
                    (is equal ,expected ,place)))))
    ;; Simple check with #'IDENTITY.
    (let ((x nil))
      (perform x :foo :foo)
      (perform x :bar :foo))
    ;; Simple check with #'IDENTITY and function init-form.
    (let ((x nil))
      (perform x (lambda () :foo) :foo :init-form-evaluates-to :function)
      (perform x (lambda () :bar) :foo :init-form-evaluates-to :function))
    ;; Simple check with #'ODDP and mixed init-forms.
    (let ((x 0))
      (perform x 2 2 :test #'oddp)
      (perform x (lambda () 4) 4 :test #'oddp :init-form-evaluates-to :function)
      (perform x 5 5 :test #'oddp)
      (perform x (lambda () 6) 5 :test #'oddp :init-form-evaluates-to :function)
      (perform x 8 5 :test #'oddp)
      (perform x (lambda () 9) 5 :test #'oddp :init-form-evaluates-to :function)
      (perform x 10 5 :test #'oddp))))

(define-test "CACHEF-OOCC-BASIC"
  :parent "CACHEF"
  (macrolet ((perform (cachedp-place place newval expected &rest cachef-args)
               (a:with-gensyms (result)
                 `(let ((,result (cachef ,cachedp-place ,place ,newval
                                         ,@cachef-args)))
                    (is equal ,expected ,result)
                    (is equal ,expected ,place)))))
    ;; Simple check with #'IDENTITY.
    (let ((x nil)
          (cachedp nil))
      (perform cachedp x :foo :foo)
      (perform cachedp x :bar :foo)
      (setf cachedp nil)
      (perform cachedp x :bar :bar))
    ;; Simple check with #'IDENTITY and function init-form.
    (let ((x nil)
          (cachedp nil))
      (perform cachedp x (lambda () :foo) :foo
               :init-form-evaluates-to :function)
      (perform cachedp x (lambda () :bar) :foo
               :init-form-evaluates-to :function)
      (setf cachedp nil)
      (perform cachedp x (lambda () :bar) :bar
               :init-form-evaluates-to :function))
    ;; Simple check with set :NEW-CACHEDP, declared types, and mixed init-forms.
    (let ((x 0)
          (cachedp nil))
      (declare (type integer x)
               (type boolean cachedp))
      (perform cachedp x 2 2 :new-cachedp nil)
      (perform cachedp x (lambda () 4) 4 :new-cachedp nil
                                         :init-form-evaluates-to :function)
      (perform cachedp x 5 5 :new-cachedp t)
      (perform cachedp x (lambda () 6) 5 :new-cachedp t
                                         :init-form-evaluates-to :function)
      (perform cachedp x 8 5 :new-cachedp t)
      (perform cachedp x (lambda () 9) 5 :new-cachedp t
                                         :init-form-evaluates-to :function)
      (perform cachedp x 10 5 :new-cachedp t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CACHEF - evaluation order test

(define-test "CACHEF-ICC-EVALUATION-ORDER-VALUE-INIT-FORM"
  :parent "CACHEF"
  (let ((*result* '())
        (x (cons nil nil)))
    (flet ((perform (expected)
             (cachef nil (loudcar x) (progn (push :init-form *result*) 42)
                     :test (progn
                             (push :test-returned *result*)
                             (lambda (x) (push :test-called *result*) x)))
             (verify expected)))
      ;; Expected order:
      ;; 1. Evaluate the test form to produce the test function.
      ;; 2. Read the initial value of the place.
      ;; 3. Call the test function on the initial value of the place.
      ;; 4. Evaluate the init form to produce the new initial value.
      ;; 5. Write the new value of the place.
      (perform '(:test-returned :place-read :test-called
                 :init-form :place-written))
      ;; Expected order:
      ;; 1. Evaluate the test form to produce the test function.
      ;; 2. Read the modified value of the place.
      ;; 3. Call the test function on the modified value of the place.
      (perform '(:test-returned :place-read :test-called)))))

(define-test "CACHEF-ICC-EVALUATION-ORDER-FUNCTION-INIT-FORM"
  :parent "CACHEF"
  (let ((*result* '())
        (x (cons nil nil)))
    (flet ((perform (expected)
             (cachef nil (loudcar x)
                     (lambda () (progn (push :init-function *result*) 42))
                     :test (progn (push :test-returned *result*)
                                  (lambda (x) (push :test-called *result*) x))
                     :init-form-evaluates-to :function)
             (verify expected)))
      ;; Expected order:
      ;; 1. Evaluate the test form to produce the test function.
      ;; 2. Read the initial value of the place.
      ;; 3. Call the test function on the initial value of the place.
      ;; 4. Call the init function to produce the new initial value.
      ;; 5. Write the new value of the place.
      (perform '(:test-returned :place-read :test-called
                 :init-function :place-written))
      ;; Expected order:
      ;; 1. Evaluate the test form to produce the test function.
      ;; 2. Read the modified value of the place.
      ;; 3. Call the test function on the modified value of the place.
      (perform '(:test-returned :place-read :test-called)))))

(define-test "CACHEF-OOCC-EVALUATION-ORDER-VALUE-INIT-FORM"
  :parent "CACHEF"
  (let ((*result* '())
        (x (cons nil nil))
        (y (cons nil nil)))
    (flet ((perform (expected)
             (cachef (loudcar y t) (loudcar x)
                     (progn (push :init-form *result*) 42)
                     :test (progn
                             (push :test-returned *result*)
                             (lambda (x) (push :test-called *result*) x)))
             (verify expected)))
      ;; Expected order:
      ;; 1. Evaluate the test form to produce the test function.
      ;; 2. Read the initial value of the cache.
      ;; 3. Call the test function on the initial value of the place.
      ;; 4. Evaluate the init form to produce the new initial value.
      ;; 5. Write the new value of the place.
      ;; 6. Write the new value of the cache.
      (perform '(:test-returned :cachedp-read :test-called
                 :init-form :place-written :cachedp-written))
      ;; Expected order:
      ;; 1. Evaluate the test form to produce the test function.
      ;; 2. Read the modified value of the cache.
      ;; 3. Call the test function on the modified value of the place.
      ;; 4. Read the modified value of the place.
      (perform '(:test-returned :cachedp-read :test-called :place-read)))))

(define-test "CACHEF-OOCC-EVALUATION-ORDER-FUNCTION-INIT-FORM"
  :parent "CACHEF"
  (let ((*result* '())
        (x (cons nil nil))
        (y (cons nil nil)))
    (flet ((perform (expected)
             (cachef (loudcar y t) (loudcar x)
                     (lambda () (progn (push :init-function *result*) 42))
                     :test (progn (push :test-returned *result*)
                                  (lambda (x) (push :test-called *result*) x))
                     :init-form-evaluates-to :function)
             (verify expected)))
      ;; Expected order:
      ;; 1. Evaluate the test form to produce the test function.
      ;; 2. Read the initial value of the cache.
      ;; 3. Call the test function on the initial value of the place.
      ;; 4. Call the init function to produce the new initial value.
      ;; 5. Write the new value of the place.
      ;; 5. Write the new value of the cache.
      (perform '(:test-returned :cachedp-read :test-called
                 :init-function :place-written :cachedp-written))
      ;; Expected order:
      ;; 1. Evaluate the test form to produce the test function.
      ;; 2. Read the modified value of the cache.
      ;; 3. Call the test function on the modified value of the place.
      ;; 4. Read the modified value of the place.
      (perform '(:test-returned :cachedp-read :test-called :place-read)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CACHEF - error tests

(define-test "CACHEF-ERROR"
  :parent "CACHEF"
  ;; CACHE-PLACE must involve only one value.
  (fail (macroexpand '(cachef nil (values foo bar baz) 42)) program-error)
  ;; CACHEDP-PLACE must involve only one value.
  (fail (macroexpand '(cachef (values foo bar baz) quux 42)) program-error)
  ;; :NEW-CACHEDP must not be provided in ICC mode.
  (fail (macroexpand '(cachef nil foo bar :new-cachedp t)) program-error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CACHEF - internal tests - utilities

(defun equal* (x y)
  (cond ((and (symbolp x) (null (symbol-package x))
              (symbolp y) (null (symbol-package y)))
         (string= x y))
        ((and (consp x) (consp y))
         (and (equal* (car x) (car y))
              (equal* (cdr x) (cdr y))))
        (t (equal x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CACHEF - internal tests

(define-test "CACHEF-INTERNAL")

(define-test "CACHEF-INTERNAL-ICC-VALUE-INIT-FORM"
  :parent "CACHEF-INTERNAL"
  (let ((*gensym-counter* 0)
        (expansion '((#:foo #:bar) (42 24) (#:new) (#:writer) (#:reader)))
        (init-form '(#:init-form))
        (test '#:test))
    (multiple-value-bind (vars vals stores writer reader)
        (place-utils::expand-icc-place expansion init-form :value test)
      (is equal* '(#:foo #:bar #:test0) vars)
      (is equal* '(42 24 #:test) vals)
      (is equal* '(#:new) stores)
      (is equal* '(#:writer) writer)
      (let ((expected-reader '(let ((#:cache-var2 (#:reader)))
                               (if (funcall #:test0 #:cache-var2)
                                   #:cache-var2
                                   (let ((#:new (#:init-form)))
                                     (#:writer))))))
        (is equal* expected-reader reader)))))

(define-test "CACHEF-INTERNAL-ICC-FUNCTION-INIT-FORM"
  :parent "CACHEF-INTERNAL"
  (let ((*gensym-counter* 0)
        (expansion '((#:foo #:bar) (42 24) (#:new) (#:writer) (#:reader)))
        (init-form '(#:init-form))
        (test '#:test))
    (multiple-value-bind (vars vals stores writer reader)
        (place-utils::expand-icc-place expansion init-form :function test)
      (is equal* '(#:foo #:bar #:init-function1 #:test0) vars)
      (is equal* '(42 24 (#:init-form) #:test) vals)
      (is equal* '(#:new) stores)
      (is equal* '(#:writer) writer)
      (let ((expected-reader `(let ((#:cache-var2 (#:reader)))
                                (if (funcall #:test0 #:cache-var2)
                                    #:cache-var2
                                    (let ((#:new (funcall #:init-function1)))
                                      (#:writer))))))
        (is equal* expected-reader reader)))))

(define-test "CACHEF-INTERNAL-OOCC-VALUE-INIT-FORM"
  :parent "CACHEF-INTERNAL"
  (let ((*gensym-counter* 0)
        (expansion-1 '((#:foo #:bar) (42 24) (#:new) (#:writer2) (#:reader2)))
        (expansion-2 '((#:baz #:qux) (51 15) (#:old) (#:writer2) (#:reader2)))
        (init-form '(#:init-form))
        (test '#:test)
        (new-cachedp '#:new-cachedp))
    (multiple-value-bind (vars vals stores writer reader)
        (place-utils::expand-oocc-place expansion-1 expansion-2
                                        init-form :value test new-cachedp)
      (is equal* '(#:foo #:bar #:baz #:qux #:test1 #:new-cachedp0)
          vars)
      (is equal* '(42 24 51 15 #:test #:new-cachedp) vals)
      (is equal* '(#:new) stores)
      (let ((expected-writer `(if (funcall #:test1 (#:writer2))
                                  (#:writer2)
                                  (prog1 (#:writer2)
                                    (let ((#:old #:new-cachedp0))
                                      (#:reader2))))))
        (is equal* expected-writer writer))
      (let ((expected-reader `(if (funcall #:test1 (#:reader2))
                                  (#:reader2)
                                  (let ((#:new (#:init-form)))
                                    (prog1 (#:writer2)
                                      (let ((#:old #:new-cachedp0))
                                        (#:writer2)))))))
        (is equal* expected-reader reader)))))

(define-test "CACHEF-INTERNAL-OOCC-FUNCTION-INIT-FORM"
  :parent "CACHEF-INTERNAL"
  (let ((*gensym-counter* 0)
        (expansion-1 '((#:foo #:bar) (42 24) (#:new) (#:writer2) (#:reader2)))
        (expansion-2 '((#:baz #:qux) (51 15) (#:old) (#:writer2) (#:reader2)))
        (init-form '(#:init-form))
        (test '#:test)
        (new-cachedp '#:new-cachedp))
    (multiple-value-bind (vars vals stores writer reader)
        (place-utils::expand-oocc-place expansion-1 expansion-2
                                        init-form :function test new-cachedp)
      (is equal* '(#:foo #:bar #:baz #:qux
                   #:init-function2 #:test1 #:new-cachedp0)
          vars)
      (is equal* '(42 24 51 15 (#:init-form) #:test #:new-cachedp) vals)
      (is equal* '(#:new) stores)
      (let ((expected-writer `(if (funcall #:test1 (#:writer2))
                                  (#:writer2)
                                  (prog1 (#:writer2)
                                    (let ((#:old #:new-cachedp0))
                                      (#:reader2))))))
        (is equal* expected-writer writer))
      (let ((expected-reader `(if (funcall #:test1 (#:reader2))
                                  (#:reader2)
                                  (let ((#:new (funcall #:init-function2)))
                                    (prog1 (#:writer2)
                                      (let ((#:old #:new-cachedp0))
                                        (#:writer2)))))))
        (is equal* expected-reader reader)))))
