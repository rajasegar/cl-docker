(defpackage docker/tests/main
  (:use :cl
        :docker
        :rove))
(in-package :docker/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :docker)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
