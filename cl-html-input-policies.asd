(asdf:defsystem #:cl-html-input-policies
  :author "Bruno Dias"
  :description "Create policies to accept html input ."
  :version #.(uiop:read-file-string "./version")
  :license #.(uiop:read-file-string "./license.md")
  :depends-on (#:io.github.cl-sdk.xml)
  :serial t
  :components ((:file "package")
               (:file "sanitizer")))

(asdf:defsystem #:cl-html-input-policies/test
  :depends-on (#:cl-html-input-policies
               #:fiveam)
  :serial t
  :components ((:file "t/package")
               (:file "t/sanitizer"))
  :perform (asdf:test-op (o c)
             (uiop:symbol-call '#:cl-html-input-policies-test '#:run-tests)))
