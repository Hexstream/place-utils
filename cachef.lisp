(in-package #:place-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CACHEF - macro

(defmacro cachef (&whole cachef-form
                    cachedp-place cache-place init-form
		              &key test new-cachedp init-form-evaluates-to)
  (declare (ignore cachedp-place cache-place init-form
		               test new-cachedp init-form-evaluates-to))
  `(readf ,cachef-form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CACHEF - main SETF expander body

(define-setf-expander cachef (cachedp-place cache-place init-form
		                          &key (test ''identity)
			                          (init-form-evaluates-to :value)
			                          (new-cachedp t new-cachedp-p)
		                          &environment env)
  (let ((cache-expansion (multiple-value-list (get-setf-expansion
                                               cache-place env))))
    (when (cdr (third cache-expansion))
	    (error 'a:simple-program-error
             :format-control "CACHE-PLACE must involve only one value."))
    (cond (cachedp-place
	         ;; Out-of-cache cachedp (OOCC) mode.
	         (let ((cachedp-expansion (multiple-value-list (get-setf-expansion
                                                          cachedp-place env))))
             (when (cdr (third cachedp-expansion))
	             (error 'a:simple-program-error
                      :format-control
                      "CACHEDP-PLACE must involve only one value."))
             (expand-oocc-place cache-expansion cachedp-expansion
                                init-form init-form-evaluates-to
                                test new-cachedp)))
          (t
           ;; In-cache cachedp (ICC) mode.
           (when new-cachedp-p
             (error 'a:simple-program-error
                    :format-control
                    "It's an error to provide :NEW-CACHEDP when CACHE-PLACE~@
                     doubles as the CACHEDP-PLACE. Presumably (but not ~@
                     necessarily) the values of INIT-FORM will satisfy TEST ~@
                     next time.~@
                     The value of :NEW-CACHEDP wrongly provided is: ~%~S"
                    :format-arguments (list new-cachedp)))
	         (expand-icc-place cache-expansion init-form
                             init-form-evaluates-to test)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CACHEF - OOCC branch, main body

(defun expand-oocc-place (cache-expansion cachedp-expansion
                          init-form init-form-evaluates-to
                          test-value new-cachedp-value)
  (a:with-gensyms (new-cachedp test init-function)
    (let* ((init-function (ecase init-form-evaluates-to
			                      (:value nil)
			                      (:function init-function)))
           ;; Return values
           (vars (oocc-generate-vars cache-expansion cachedp-expansion
                                     init-function new-cachedp test))
           (vals (oocc-generate-vals cache-expansion cachedp-expansion
                                     init-function init-form
                                     new-cachedp-value test-value))
           (stores (third cache-expansion))
           (writer (oocc-generate-writer cache-expansion cachedp-expansion
                                         new-cachedp test))
           (reader (oocc-generate-reader cache-expansion cachedp-expansion
                                         init-function init-form
                                         new-cachedp test)))
	    (values vars vals stores writer reader))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CACHEF - OOCC branch, generating return values

(defun oocc-generate-vars (cache-expansion cachedp-expansion init-function
                           new-cachedp-var test-var)
  `(,@(first cache-expansion)
    ,@(first cachedp-expansion)
	  ,@(when init-function (list init-function))
    ,test-var ,new-cachedp-var))

(defun oocc-generate-vals (cache-expansion cachedp-expansion init-function
                           init-form new-cachedp test)
  `(,@(second cache-expansion)
    ,@(second cachedp-expansion)
		,@(when init-function (list init-form))
    ,test ,new-cachedp))

(defun oocc-generate-caching (cache-writer cachedp-stores
                              cachedp-writer new-cachedp-var)
  `(prog1 ,cache-writer
		 (let ((,(car cachedp-stores) ,new-cachedp-var))
			 ,cachedp-writer)))

(defun oocc-generate-writer (cache-expansion cachedp-expansion
                             new-cachedp-var test-var)
  (let ((cache-writer (fourth cache-expansion)))
    (destructuring-bind (cachedp-stores cachedp-reader cachedp-writer)
        (cddr cachedp-expansion)
      `(if (funcall ,test-var ,cachedp-reader)
			     ,cache-writer
           ,(oocc-generate-caching cache-writer cachedp-stores
                                   cachedp-writer new-cachedp-var)))))

(defun oocc-generate-reader (cache-expansion cachedp-expansion
                             init-function init-form
                             new-cachedp-var test-var)
  (destructuring-bind (cache-stores cache-writer cache-reader)
      (cddr cache-expansion)
    (destructuring-bind (cachedp-stores cachedp-writer cachedp-reader)
        (cddr cachedp-expansion)
      (let ((evaluate-init-form (if init-function
                                    `(funcall ,init-function)
                                    init-form)))
        `(if (funcall ,test-var ,cachedp-reader)
		         ,cache-reader
		         (let ((,(car cache-stores) ,evaluate-init-form))
			         ,(oocc-generate-caching cache-writer cachedp-stores
                                       cachedp-writer new-cachedp-var)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CACHEF - ICC branch, main body

(defun expand-icc-place (cache-expansion init-form
                         init-form-evaluates-to test-value)
  (a:with-gensyms (test init-function)
    (let* ((init-function (ecase init-form-evaluates-to
			                      (:value nil)
			                      (:function init-function)))
	         (evaluate-init-form (if init-function
				                           `(funcall ,init-function)
				                           init-form))
           (vars (icc-generate-vars cache-expansion init-function
                                    test))
           (vals (icc-generate-vals cache-expansion init-function
                                    init-form test-value))
           (stores (third cache-expansion))
           (writer (fourth cache-expansion))
           (reader (icc-generate-reader cache-expansion evaluate-init-form
                                        test)))
      (values vars vals stores writer reader))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CACHEF - ICC branch, generating return values

(defun icc-generate-vars (cache-expansion init-function test-var)
  `(,@(first cache-expansion)
		,@(when init-function (list init-function))
		,test-var))

(defun icc-generate-vals (cache-expansion init-function init-form test)
  `(,@(second cache-expansion)
		,@(when init-function (list init-form))
		,test))

(defun icc-generate-reader (cache-expansion evaluate-init-form test-var)
  (destructuring-bind (cache-stores cache-writer cache-reader)
      (cddr cache-expansion)
    (a:with-gensyms (cache-var)
      `(let ((,cache-var ,cache-reader))
		     (if (funcall ,test-var ,cache-var)
			       ,cache-var
			       (let ((,(car cache-stores) ,evaluate-init-form))
				       ,cache-writer))))))
