(in-package #:cl-user)
(defpackage #:place-utils.system
  (:use #:cl #:asdf))
(in-package #:place-utils.system)


(defsystem place-utils
  :author "Jean-Philippe Paradis <hexstream@gmail.com>"
  :version "0.1"
  :description "Provides a few utilities relating to setfable places."
  :components ((:file "package")
	       (:file "main" :depends-on ("package"))))
