(in-package #:place-utils)

;;;; WIP for 0.2.

(defmacro while (test &body body)
  `(prog ()
    loop-start
      (if (not ,test)
	  (return))
      (prog () ,@body)
      (go loop-start)))

; vals-transformer
#+nil(flet ((wrap (value &rest conditions-and-wrappers)
	      (while conditions-and-wrappers
		(destructuring-bind (condition wrapper &rest rest)
		    conditions-and-wrappers
		  (when condition
		    (setf value (funcall wrapper value)))
		  (setf conditions-and-wrappers
			rest)))
	      value))
       (if (or before-subform around-subform after-subform)
	   (mapcar
	    (lambda (var val)
	      (wrap val
		    after-subform
		    (lambda (val)
		      `(funcall ,after-subform
				',var
				',val
				,val))
		    subform
		    (lambda (val)
		      `(funcall ,subform
				',var
				',val
				(lambda ()
				  ,val))
		      (wrap-if 
		       val))
		    before-subform
		    (lambda (val)
		      `(progn (funcall ,before-subform
				       ',var
				       ',val)
			      ,val))))
	    vars vals)
	   vals))

; writer-transformer
#+nil
(let ((writer (if after-write
		  ?
		  writer)))
  (if before-write
      `(multiple-value-bind (,@stores)
	   (funcall ,before-write ,@stores)
	 writer)
      writer))

; read-transformer
#+nil
(if read
    `(multiple-value-call ,read ,reader)
    reader)

#+nil(flet
	 ((chain (proxies)
	    (if proxies
		(destructuring-bind (current . rest)
		    proxies
		  (if rest
		      (let ((rest (chain rest)))
			(lambda (&rest args)
			  (multiple-value-call rest
			    (apply current args))))
		      current))
		nil)))
       )

(declaim (type list *next-proxies* *proxy-args*))
(defparameter *next-proxies* nil)
(defparameter *proxy-args* nil)


(defun call-next-customizer (&rest args)
  (let ((nexts *next-proxies*))
    (if nexts
	(let* ((*next-proxies* (cdr nexts))
	       (*proxy-args* (or args *proxy-args*)))
	  (apply (car nexts) *proxy-args*))
	(error "Attempted to call call-next-customizer outside ~
                the dynamic extent of a proxyf around customizer."))))

(define-setf-expander proxyf
    ((&rest proxies
	    &key
	    before-subform around-subform after-subform
	    before-write around-write after-write
	    before-read around-read after-read)
     place
     &environment env)
  (unless proxies
    (return-from proxyf (get-setf-expansion place env)))
  (macrolet
      ((with-proxies (groups &body body)
	 (multiple-value-bind (vars returns accumulations)
	     (let (vars returns accumulations)
	       (dolist (binding
			 (mapcan
			  (lambda (group)
			    (destructuring-bind (before around after)
				group
			      `((,before :first)
				(,around :first)
				(,after :last))))
			  groups)
			(values (nreverse vars)
				(nreverse returns)
				(nreverse accumulations)))
		 (destructuring-bind (var most-specific)
		     binding
		   (push var vars)
		   (push (ecase most-specific
			   (:first `(nreverse ,var))
			   (:last var))
			 returns)
		   (push `(,(intern (symbol-name var) '#:keyword)
			    (let ((proxy-var
				   (gensym ,(symbol-name var))))
			      (push proxy-var proxy-vars)
			      (push proxy proxy-vals)
			      (push proxy-var ,var)))
			 accumulations))))
	   `(multiple-value-bind (proxy-vars proxy-vals ,@vars)
		(let (proxy-vars proxy-vals ,@vars)
		  (do ((proxies proxies (cddr proxies)))
		      ((null proxies) (values (nreverse proxy-vars)
					      (nreverse proxy-vals)
					      ,@returns))
		    (destructuring-bind (kind proxy &rest rest)
			proxies
		      (declare (ignore rest))
		      (case kind
			,@accumulations))))
	      ,@body))))
    (with-proxies ((before-subform around-subform after-subform)
		   (before-write around-write after-write)
		   (before-read around-read after-read))
      (flet ((transform-subform (var val)
	       (setf
		val
		(if after
			 
		    val)
		val
		(if before
		    `(progn
		       ,(mapcar (lambda (before)
				  `(funcall ,before ',var ',val))
				before-subform)
		       val)
		    val)
		val
		(if around
		    `(let ((*proxy-args* )
			   (*next-proxies* ))
		       (funcall ,around ',var ',val))
		    val))
	       val)
	     (transform-writer (stores writer)
	       )
	     (transform-reader (reader)
	       (let ((before (effective-before-proxy before-reader))
		     (around)
		     (after))
		 (setf reader (if around
				  `(multiple-value-call ,around ,reader)
				  reader)
		       reader (if after-read
				  `(multiple-value-prog1 reader
				     ())
				  reader)))
	       reader))
	(let ((subform-proxy-var ))
	  (multiple-value-bind (vars vals stores writer reader)
	      (get-setf-expansion place env)
	    (values (append proxy-vars
			    (when subform-proxy-var
			      (list subform-proxy-var))
			    vars)
		    (append proxy-vals
			    (if subform-proxy-var
				(mapcar
				 (lambda (var val)
				   `(funcall ,subform-proxy-var
					     ',var ',val))
				 vars vals)
				vals)
			    )
		    stores
		    (transform-writer stores writer)
		    (transform-reader reader))))))))

(defmacro proxyf (&whole whole
		  (&key
		   before-subform around-subform after-subform
		   before-write around-write after-write
		   before-read around-read after-read)
		  place)
  (declare (ignore before-subform around-subform after-subform
		   before-write around-write after-write
		   before-read around-read after-read
		   place))
  `(readf ,whole))
