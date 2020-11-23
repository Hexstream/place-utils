(asdf:defsystem #:place-utils

  :author "Jean-Philippe Paradis <hexstream@hexstreamsoft.com>"

  :license "Unlicense"

  :description "Provides a few utilities relating to setfable places."

  :version "0.2"
  :serial cl:t
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:file "main")
               (:file "cachef"))

  :in-order-to ((asdf:test-op (asdf:test-op #:place-utils_tests))))
