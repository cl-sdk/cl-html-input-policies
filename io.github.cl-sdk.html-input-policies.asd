(asdf:defsystem #:io.github.cl-sdk.html-input-policies
  :author "Bruno Dias"
  :description "Create policies to accept HTML input."
  :long-description #.(uiop:read-file-string "./readme.md")
  :version #.(uiop:read-file-string "./version")
  :license #.(uiop:read-file-string "./license.md")
  :homepage "https://github.com/cl-sdk/io.github.cl-sdk.html-input-policies"
  :source-control (:git "https://github.com/cl-sdk/io.github.cl-sdk.html-input-policies")
  :bug-tracker "https://github.com/cl-sdk/io.github.cl-sdk.html-input-policies/issues"
  :depends-on (#:io.github.cl-sdk.xml)
  :in-order-to ((test-op (test-op #:io.github.cl-sdk.html-input-policies.test)))
  :serial t
  :components ((:file "package")
	       (:file "sanitizer")))
