(in-package #:cl-user)
(defpackage #:place-utils.system
  (:use #:cl #:asdf))
(in-package #:place-utils.system)


(defsystem place-utils
  :author "Jean-Philippe Paradis <hexstream@gmail.com>"
  :version "0.1"
  :components ((:file "package")
	       (:file "main" :depends-on ("package"))))
