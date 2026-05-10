(push *default-pathname-defaults* ql:*local-project-directories*)

(setf asdf/source-registry::*source-registry-file* #P"./.qlot/")

(asdf:initialize-source-registry)

(ql:quickload :io.github.cl-sdk.html-input-policies.test)

(unless (5am:run-all-tests)
  (uiop:quit -1))
