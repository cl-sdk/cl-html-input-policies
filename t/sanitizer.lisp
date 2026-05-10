(in-package #:cl-html-input-policies-test)

(test removes-dangerous-denied-tag-and-content
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
