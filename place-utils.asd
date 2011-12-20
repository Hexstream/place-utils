(asdf:defsystem #:place-utils

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "Provides utilities relating to setfable places: a few novel ones and a good number of trivial, traditional ones."

  :version "0.2"
  :serial cl:t
  :components ((:file "package")
	       (:file "enhanced")
	       (:file "novel")
	       #+nil(:file "traditional")))
