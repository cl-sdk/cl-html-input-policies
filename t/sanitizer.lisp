(in-package #:cl-html-input-policies-test)

(def-suite html-input-policies-suite)
(in-suite html-input-policies-suite)

(defun make-test-node (tag &key (attributes nil) (children nil))
  (io.github.cl-sdk.xml:make-xml-node
   :tag tag
   :attributes attributes
   :children children))

(defun make-test-doc (root)
  (io.github.cl-sdk.xml:make-xml-document
   :prolog nil
   :doctype nil
   :root root))

(test removes-script-tag-and-content
  (let* ((script (make-test-node "script" :children '("alert(1)")))
         (root (make-test-node "p" :children (list "Hello" script "World"))))
    (is (string=
         "<p>HelloWorld</p>"
         (sanitize-html-input (make-test-doc root) '("script"))))))

(test removes-denied-tag-but-keeps-its-text-content
  (let* ((b (make-test-node "b" :children '("Hello")))
         (i (make-test-node "i" :children '("world")))
         (root (make-test-node "p" :children (list b " " i))))
    (is (string=
         "<p>Hello world</p>"
         (sanitize-html-input (make-test-doc root) '("b" "i"))))))

(test keeps-allowed-tags
  (let* ((em (make-test-node "em" :children '("safe")))
         (root (make-test-node "p" :children (list em " text"))))
    (is (string=
         "<p><em>safe</em> text</p>"
         (sanitize-html-input (make-test-doc root) '("script"))))))

(test removes-denied-self-closing-tags
  (let* ((br (make-test-node "br"))
         (root (make-test-node "root" :children (list "Hello" br "World"))))
    (is (string=
         "HelloWorld"
         (sanitize-html-input (make-test-doc root) '("br" "root"))))))

(test removes-self-closing-dangerous-tag
  (let* ((script (make-test-node "script"))
         (root (make-test-node "root" :children (list script "safe"))))
    (is (string=
         "safe"
         (sanitize-html-input (make-test-doc root) '("script" "root"))))))

(test tag-check-is-case-insensitive
  (let* ((script (make-test-node "SCRIPT" :children '("alert(1)")))
         (root (make-test-node "div" :children (list script))))
    (is (string=
         "<div/>"
         (sanitize-html-input (make-test-doc root) '("script"))))))

(test removes-nested-script-tags-with-content
  (let* ((inner (make-test-node "script" :children '("2")))
         (outer (make-test-node "script" :children (list "1" inner "3")))
         (root (make-test-node "p" :children (list outer "x"))))
    (is (string=
         "<p>x</p>"
         (sanitize-html-input (make-test-doc root) '("script"))))))

(test removes-script-tags-with-attributes-and-content
  (let* ((script (make-test-node "script"
                                 :attributes '(("src" . "evil.js"))
                                 :children '("alert(1)")))
         (root (make-test-node "p" :children (list script "ok"))))
    (is (string=
         "<p>ok</p>"
         (sanitize-html-input (make-test-doc root) '("script"))))))

(test accepts-xml-document-structures-as-input
  (let* ((script-node (io.github.cl-sdk.xml:make-xml-node
                       :tag "script"
                       :attributes nil
                       :children '("alert(1)")))
         (p-node (io.github.cl-sdk.xml:make-xml-node
                  :tag "p"
                  :attributes nil
                  :children (list "Hello" script-node "World")))
         (doc (io.github.cl-sdk.xml:make-xml-document
               :prolog nil
               :doctype nil
               :root p-node)))
    (is (string=
         "<p>HelloWorld</p>"
         (sanitize-html-input doc '("script"))))))
