(asdf:defsystem #:io.github.cl-sdk.html-input-policies.test
  :depends-on (#:io.github.cl-sdk.html-input-policies
               #:fiveam)
  :serial t
  :components ((:file "t/package")
               (:file "t/sanitizer")))
