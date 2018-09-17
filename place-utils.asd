(asdf:defsystem #:place-utils

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "Provides a few utilities relating to setfable places."

  :version "0.2"
  :serial cl:t
  :components ((:file "package")
	       (:file "main"))

  :in-order-to ((asdf:test-op (asdf:test-op #:place-utils_tests))))
