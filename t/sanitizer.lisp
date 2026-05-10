(in-package #:cl-html-input-policies-test)

(def-suite html-input-policies-suite)
(in-suite html-input-policies-suite)

(test removes-script-tag-and-content
  (is (string=
       "<p>HelloWorld</p>"
       (sanitize-html-input "<p>Hello<script>alert(1)</script>World</p>"
                            '("script")))))

(test removes-denied-tag-but-keeps-its-text-content
  (is (string=
       "<p>Hello world</p>"
       (sanitize-html-input "<p><b>Hello</b> <i>world</i></p>"
                            '("b" "i")))))

(test keeps-allowed-tags
  (is (string=
       "<p><em>safe</em> text</p>"
       (sanitize-html-input "<p><em>safe</em> text</p>"
                            '("script")))))

(test removes-denied-self-closing-tags
  (is (string=
       "HelloWorld"
       (sanitize-html-input "Hello<br/>World"
                            '("br")))))

(test removes-self-closing-dangerous-tag
  (is (string=
       "safe"
       (sanitize-html-input "<script/>safe"
                            '("script")))))

(test tag-check-is-case-insensitive
  (is (string=
       ""
       (sanitize-html-input "<SCRIPT>alert(1)</SCRIPT>"
                            '("script")))))

(test removes-nested-script-tags-with-content
  (is (string=
       "<p>x</p>"
       (sanitize-html-input "<script>1<script>2</script>3</script><p>x</p>"
                            '("script")))))

(test removes-script-tags-with-attributes-and-content
  (is (string=
       "<p>ok</p>"
       (sanitize-html-input "<script src=\"evil.js\">alert(1)</script><p>ok</p>"
                            '("script")))))

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
