(in-package #:cl-html-input-policies-test)

(def-suite html-input-policies-suite)
(in-suite html-input-policies-suite)

(defun node (tag &key (attributes nil) (children nil))
  (io.github.cl-sdk.xml:make-xml-node
   :tag tag
   :attributes attributes
   :children children))

(defun doc (root)
  (io.github.cl-sdk.xml:make-xml-document
   :prolog nil
   :doctype nil
   :root root))

(test removes-script-tag-and-content
  (let* ((script (node "script" :children '("alert(1)")))
         (root (node "p" :children (list "Hello" script "World"))))
    (is (string=
         "<p>HelloWorld</p>"
         (sanitize-html-input (doc root) '("script"))))))

(test removes-denied-tag-but-keeps-its-text-content
  (let* ((b (node "b" :children '("Hello")))
         (i (node "i" :children '("world")))
         (root (node "p" :children (list b " " i))))
    (is (string=
         "<p>Hello world</p>"
         (sanitize-html-input (doc root) '("b" "i"))))))

(test keeps-allowed-tags
  (let* ((em (node "em" :children '("safe")))
         (root (node "p" :children (list em " text"))))
    (is (string=
         "<p><em>safe</em> text</p>"
         (sanitize-html-input (doc root) '("script"))))))

(test removes-denied-self-closing-tags
  (let* ((br (node "br"))
         (root (node "p" :children (list "Hello" br "World"))))
    (is (string=
         "<p>HelloWorld</p>"
         (sanitize-html-input (doc root) '("br"))))))

(test removes-self-closing-dangerous-tag
  (let* ((script (node "script"))
         (root (node "p" :children (list script "safe"))))
    (is (string=
         "<p>safe</p>"
         (sanitize-html-input (doc root) '("script"))))))

(test tag-check-is-case-insensitive
  (let* ((script (node "SCRIPT" :children '("alert(1)")))
         (root (node "div" :children (list script))))
    (is (string=
         "<div/>"
         (sanitize-html-input (doc root) '("script"))))))

(test removes-nested-script-tags-with-content
  (let* ((inner (node "script" :children '("2")))
         (outer (node "script" :children (list "1" inner "3")))
         (root (node "p" :children (list outer "x"))))
    (is (string=
         "<p>x</p>"
         (sanitize-html-input (doc root) '("script"))))))

(test removes-script-tags-with-attributes-and-content
  (let* ((script (node "script"
                       :attributes '(("src" . "evil.js"))
                       :children '("alert(1)")))
         (root (node "p" :children (list script "ok"))))
    (is (string=
         "<p>ok</p>"
         (sanitize-html-input (doc root) '("script"))))))

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
