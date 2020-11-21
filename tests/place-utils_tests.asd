(asdf:defsystem #:place-utils_tests

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  :license "Unlicense"

  :description "place-utils unit tests."

  :depends-on ("place-utils"
               "parachute")

  :serial cl:t
  :components ((:file "tests"))

  :perform (asdf:test-op (op c) (uiop:symbol-call '#:parachute '#:test '#:place-utils_tests)))
