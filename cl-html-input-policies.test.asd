(asdf:defsystem #:cl-html-input-policies.test
  :depends-on (#:cl-html-input-policies
               #:fiveam)
  :serial t
  :components ((:file "t/package")
               (:file "t/sanitizer")))
