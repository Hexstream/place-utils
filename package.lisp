(in-package #:cl-user)

(defpackage #:place-utils
  (:nicknames #:place)
  (:use #:cl)
  (:shadow #:define-modify-macro)
  ;; Novel
  (:export #:&place
	   #:define-modify-macro
	   #:define-modify-macro*
	   #:modify
	   #:setf-expanderlet
	   #:with-resolved-places
	   
	   #:updatef
	   #:bulkf
	   ;#:setfnew

	   ;#:proxyf
	   ;#:call-next-customizer
	   #:cachef
	   #:oldf
	   #:readf
	   #:tracef))
