(asdf:defsystem #:io.github.cl-sdk.html-input-policies.test
  :author "Bruno Dias"
  :description "Tests for io.github.cl-sdk.html-input-policies."
  :version #.(uiop:read-file-string "./version")
  :license #.(uiop:read-file-string "./license.md")
  :depends-on (#:io.github.cl-sdk.html-input-policies
               #:fiveam)
  :perform (test-op (op c)
             (uiop:symbol-call :fiveam '#:run!
               (uiop:find-symbol* '#:html-input-policies-suite
                 '#:io.github.cl-sdk.html-input-policies.test)))
  :serial t
  :components ((:file "t/package")
               (:file "t/sanitizer")))
