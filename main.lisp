(in-package #:place-utils)

(define-setf-expander setf-expanderlet-helper (vars values stores writer reader)
  (values vars values stores writer reader))

(defun setf-expanderlet-values (vars values stores writer reader)
  `(setf-expanderlet-helper ,vars ,values ,stores ,writer ,reader))

(defparameter *really-expand* nil)

(defun reader-from-setf-expansion (vars values stores writer reader)
  (declare (ignore stores writer))
  `(let* (,@(mapcar #'list vars values))
     ,reader))

(defmacro %readf (place &environment env)
  (multiple-value-call #'reader-from-setf-expansion
    (let ((*really-expand* t))
      (get-setf-expansion place env))))

(define-setf-expander %readf (place &environment env)
  (let ((*really-expand* t))
    (get-setf-expansion place env)))

(defmacro setf-expanderlet (bindings &body body)
  (let ((macros
	 (mapcar
	  (lambda (binding)
	    (destructuring-bind (name lambda-list &body body)
		binding
	      (let* ((wholep
		      (let ((lambda-list
			     (if (eq (car lambda-list) '&environment)
				 (cddr lambda-list)
				 lambda-list)))
			(when (eq (car lambda-list) '&whole)
			  (or (symbolp (second lambda-list))
			      (error "Sorry, setf-expanderlet doesn't ~
                                      support &whole var destructuring.")))))
		     (whole (if wholep
				(second lambda-list)
				(gensym "WHOLE")))
		     (lambda-list (append (unless wholep `(&whole ,whole))
					  lambda-list)))
		`(,name ,lambda-list
			(if *really-expand*
			    (multiple-value-call
				#'setf-expanderlet-values
			      (locally ,@body))
			    `(%readf ,,whole))))))
	  bindings)))
    `(macrolet (,@macros)
       ,@body)))

;; Support for partial resolving would be really nice...
(defmacro with-resolved-places (bindings &body body &environment env)
  (multiple-value-bind (let*-bindings
			local-expanders
			macrolet-bindings)
      (let (let*-bindings local-expanders macrolet-bindings)
	(dolist (binding bindings
		 (values (nreverse let*-bindings)
			 (nreverse local-expanders)
			 (nreverse macrolet-bindings)))
	  (destructuring-bind (name place) binding
	    (multiple-value-bind (pvars pvalues stores writer reader)
		(get-setf-expansion place env)
	      (mapc (lambda (pvar pvalue)
		      (push `(,pvar ,pvalue) let*-bindings))
		    pvars pvalues)
	      (let ((expander-name (gensym (symbol-name name))))
		(push `(,expander-name ()
				       (values 'nil
					       'nil
					       ',stores
					       ',writer
					       ',reader))
		      local-expanders)
		(push `(,name (,expander-name))
		      macrolet-bindings))))))
    `(let* (,@let*-bindings)
       (declare (ignorable ,@(mapcar #'car let*-bindings)))
       (setf-expanderlet (,@local-expanders)
	 (symbol-macrolet (,@macrolet-bindings)
	   ,@body)))))

(defmacro readf (place &environment env)
  (multiple-value-call #'reader-from-setf-expansion
    (get-setf-expansion place env)))

(define-setf-expander readf (place &environment env)
  (get-setf-expansion place env))

(defmacro updatef (&rest args &environment env)
  (do ((args args (cddr args))
       forms)
      ((null args) (when forms `(progn ,@(nreverse forms))))
    (unless (cdr args)
      (error "Odd number of arguments given to UPDATEF."))
    (destructuring-bind (place update-functions &rest rest) args
      (declare (ignore rest))
      (multiple-value-bind (subvars subforms stores writer reader)
	  (get-setf-expansion place env)
	(push `(let* (,@(mapcar #'list subvars subforms))
		 (multiple-value-bind (,@stores)
		     (values-list (mapcar (lambda (old-value update-function)
					    (funcall update-function old-value))
					  (multiple-value-list ,reader)
					  (multiple-value-list ,update-functions)))
		   ,writer))
	      forms)))))

(defmacro bulkf (&whole whole update-function
		 &rest mode-markers-and-items
		 &environment env)
  (declare (ignore update-function mode-markers-and-items))
  (destructuring-bind (funcall-or-apply update-function
		       &rest mode-markers-and-items)
      (if (member (second whole) '(funcall apply))
	  (cdr whole)
	  `(funcall ,@(cdr whole)))
    (let ((update (gensym "UPDATE")))
      (multiple-value-bind (let*-bindings
			    used-stores
			    unused-stores
			    update-args
			    writers)
	  (let (let*-bindings
		used-stores
		unused-stores
		update-args
		writers)
	    (push `(,update ,update-function) let*-bindings)
	    (let ((mode :access))
	      (dolist (marker-or-item mode-markers-and-items
		       (values-list (mapcar #'nreverse
					    (list let*-bindings
						  used-stores
						  unused-stores
						  update-args
						  writers))))
		(if (member marker-or-item
			    '(:access :read :write :pass))
		    (setf mode marker-or-item)
		    (if (member mode '(:access :read :write))
			(multiple-value-bind
			      (pvars pvals stores writer reader)
			    (get-setf-expansion marker-or-item env)
			  (mapc (lambda (var val)
				  (push `(,var ,val) let*-bindings))
				pvars pvals)
			  (flet
			      ((acc-read (var)
				 (push `(,var ,reader) let*-bindings)
				 (push var update-args))
			       (acc-write ()
				 (when stores
				   (push (car stores) used-stores)
				   (dolist (store (cdr stores))
				     (push store unused-stores)))
				 (push writer writers)))
			    (ecase mode
			      (:access
				 (acc-read (gensym "ACCESSED"))
				 (acc-write))
			      (:read
				 (acc-read (gensym "READ")))
			      (:write
				 (acc-write)))))
			(let ((passed (gensym "PASSED")))
			  (push `(,passed ,marker-or-item)
				let*-bindings)
			  (push passed update-args)))))))
	`(let* (,@let*-bindings)
	   (multiple-value-bind (,@used-stores)
	       (,funcall-or-apply ,update ,@update-args)
	     (let (,@unused-stores)
	       (values ,@writers))))))))

(defmacro funcallf (function place &rest other-args)
  `(bulkf funcall ,function ,place :pass ,@other-args))

(defmacro applyf (function place &rest other-args)
  `(bulkf apply ,function ,place :pass ,@other-args))

(define-setf-expander cachef (cachedp-place
			      cache-place
			      init-form
			      &rest key-args
			      &key test
			      (init-form-evaluates-to :value)
			      (new-cachedp t new-cachedp-sp)
			      &environment env)
  (let* ((test (or test '#'identity))
	 (test-var (gensym "TEST"))
	 (init-form-function (ecase init-form-evaluates-to
			       (:value nil)
			       (:function (gensym "INIT-FORM-FUNCTION"))))
	 (evaluate-init-form (if init-form-function
				 `(funcall ,init-form-function)
				 init-form)))
    (multiple-value-bind (cache-vars cache-values cache-stores
			  cache-writer cache-reader)
	(get-setf-expansion cache-place env)
      (when (cdr cache-stores)
	(error "cache-place must involve only one value."))
      (if cachedp-place
	  ;; Out-of-cache cachedp (OOCC) mode.
	  (multiple-value-bind (cachedp-vars cachedp-values cachedp-stores
				cachedp-writer cachedp-reader)
	      (get-setf-expansion cachedp-place env)
	    (when (cdr cachedp-stores)
	      (error "cachedp-place must involve only one value."))
	    (let* ((new-cachedp-var (gensym "NEW-CACHEDP"))
		   (do-caching
		       `(prog1 ,cache-writer
			  (let ((,(car cachedp-stores) ,new-cachedp-var))
			    ,cachedp-writer)))
		   (correct-order
		    (ecase (do ((key-args key-args (cddr key-args)))
			       ((null key-args) :test)
			     (let ((key (car key-args)))
			       (when (member key '(:test :new-cachedp))
				 (return key))))
		      (:test #'identity)
		      (:new-cachedp #'nreverse))))
	      (values `(,@cache-vars
			,@cachedp-vars
			,@(when init-form-function
			    (list init-form-function))
			,@(funcall correct-order
				   (list test-var new-cachedp-var)))
		      `(,@cache-values
			,@cachedp-values
			,@(when init-form-function
			    (list init-form))
			,@(funcall correct-order
				   (list test new-cachedp)))
		      cache-stores
		      `(if (funcall ,test-var ,cachedp-reader)
			   ,cache-writer
			   ,do-caching)
		      `(if (funcall ,test-var ,cachedp-reader)
			   ,cache-reader
			   (let ((,(car cache-stores) ,evaluate-init-form))
			     ,do-caching)))))
	  ;; In-cache cachedp (ICC) mode.
	  (if new-cachedp-sp
	      (error "It's an error to provide :NEW-CACHEDP~@
                      when CACHE-PLACE doubles as the CACHEDP-PLACE.~@
                      Presumably (but not necessarily) the values of INIT-FORM ~
                      will satisfy TEST next time.~@
                      The value of :NEW-CACHEDP wrongly provided is: ~%~S"
		     new-cachedp)
	      (let ((cache-value (gensym "CACHE-VALUE")))
		(values `(,@cache-vars
			  ,@(when init-form-function
			      (list init-form-function))
			  ,test-var)
			`(,@cache-values
			  ,@(when init-form-function
			      (list init-form))
			  ,test)
			cache-stores
			cache-writer
			`(let ((,cache-value ,cache-reader))
			   (if (funcall ,test-var ,cache-value)
			       ,cache-value
			       (let ((,(car cache-stores) ,evaluate-init-form))
				 ,cache-writer))))))))))

(defmacro cachef (&whole cachef-form
		  cachedp-place cache-place init-form
		  &key test new-cachedp init-form-evaluates-to)
  (declare (ignore cachedp-place cache-place init-form
		   test new-cachedp init-form-evaluates-to))
  `(readf ,cachef-form))

(define-setf-expander oldf (place &environment env)
  (multiple-value-bind (subvars subforms stores writer reader)
      (get-setf-expansion place env)
    (values subvars
	    subforms
	    stores
	    `(multiple-value-prog1 ,reader
	       ,writer)
	    reader)))

(defmacro oldf (&whole whole place)
  (declare (ignore place))
  `(readf ,whole))

(define-setf-expander tracef (place &environment env)
  (multiple-value-bind (vars values stores writer reader)
      (get-setf-expansion place env)
    (flet ((fmt (action control-string &rest format-args)
	     `(format *trace-output*
		      "~&TRACEF: Place: ~S~@
                       TRACEF: Action: ~A~@
                       ~A~2%"
		      ',place ,action
		      (format nil ,control-string ,@format-args))))
      (let ((result (gensym "RESULT")))
	(values vars
		(mapcar
		 (lambda (value)
		   `(let ((,result ,value))
		      ,(fmt "Evaluate Subform"
			    "TRACEF: Subform: ~S~@
                             TRACEF: Result: ~S"
			    `',value result)
		      ,result))
		 values)
		stores
		`(progn ,(fmt "Write"
			      "TRACEF: Values: ~S"
			      `(list ,@stores))
			,writer)
		`(let ((,result (multiple-value-list ,reader)))
		   ,(fmt "Read"
			 "TRACEF: Values: ~S"
			 result)
		   (values-list ,result)))))))

(defmacro tracef (&whole whole place)
  (declare (ignore place))
  `(readf ,whole))


;; Doesn't handle declarations at this time.
;; I'm not sure this really belongs here...
#+nil
(defmacro lazy-let (bindings &body body)
  (multiple-value-bind (cachedp-vars
			cache-vars
			cache-bindings
			resolved-places)
      (let (cachedp-vars cache-vars cache-bindings resolved-places)
	(dolist (binding bindings
		 (values (nreverse cachedp-vars)
			 (nreverse cache-vars)
			 (nreverse cache-bindings)
			 (nreverse resolved-places)))
	  (destructuring-bind (var init-form
			       &key (placeholder nil placeholderp))
	      (if (symbolp binding)
		  (list binding nil)
		  binding)
	    (let ((cachedp-var (gensym "CACHEDP"))
		  (cache-var (gensym "CACHE")))
	      (push cachedp-var cachedp-vars)
	      (push cache-var cache-vars)
	      (push (if placeholderp
			`(,cache-var ,placeholder)
			cache-var)
		    cache-bindings)
	      (push `(,var (cachef ,cachedp-var
				   ,cache-var
				   (lambda ()
				     ,init-form)
				   :init-form-evaluates-to :function))
		    resolved-places)))))
    `(let (,@(mapcan #'list cachedp-vars cache-bindings))
       (declare (ignorable ,@(mapcan #'list
				     cachedp-vars
				     cache-vars))
		(type boolean ,@cachedp-vars))
       (with-resolved-places (,@resolved-places)
	 ,@body))))
