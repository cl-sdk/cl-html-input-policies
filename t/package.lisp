(defpackage #:cl-html-input-policies-test
  (:use #:cl #:fiveam #:cl-html-input-policies)
  (:export #:run-tests))

(in-package #:cl-html-input-policies-test)

(def-suite html-input-policies-suite)
(in-suite html-input-policies-suite)

(defun run-tests ()
  (run! 'html-input-policies-suite))
