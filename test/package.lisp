(defpackage #:beadwork/tests
  (:use #:cl #:parachute))

(in-package #:beadwork/tests)

(define-test beadwork-suite
  :parent NIL
  :description "Root suite for all beadwork tests")
