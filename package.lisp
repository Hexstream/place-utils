(in-package #:cl-user)

(defpackage #:place-utils
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export #:setf-expanderlet
           #:with-resolved-places
           #:updatef
           #:bulkf
           #:cachef
           #:oldf
           #:readf
           #:tracef))
