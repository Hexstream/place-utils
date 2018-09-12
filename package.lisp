(in-package #:cl-user)

(defpackage #:place-utils
  (:use #:cl)
  (:export #:setf-expanderlet
	   #:with-resolved-places
	   #:updatef
	   #:bulkf
           #:funcallf
           #:applyf
	   #:cachef
	   #:oldf
	   #:readf
	   #:tracef))
