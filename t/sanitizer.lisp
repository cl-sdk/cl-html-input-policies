(in-package #:cl-html-input-policies.test)

(def-suite html-input-policies-suite)
(in-suite html-input-policies-suite)

(defun make-test-node (tag &key (attributes nil) (children nil))
  "Create an XML node for sanitizer policy tests."
  (io.github.cl-sdk.xml:make-xml-node
   :tag tag
   :attributes attributes
   :children children))

(defun make-test-doc (root)
  "Create a minimal XML document with ROOT for sanitizer policy tests."
  (io.github.cl-sdk.xml:make-xml-document
   :prolog nil
   :doctype nil
   :root root))

(defun attribute-value->string (value)
  "Normalize attribute VALUE to the string representation used by renderer."
  (if value (princ-to-string value) ""))

(defun render-sanitized-fragment (fragment)
  "Render a sanitized FRAGMENT (strings and xml-node entries) to an XML string."
  (labels ((render-node (node stream)
             (let ((tag-name (cl-html-input-policies::%xml-name->string (io.github.cl-sdk.xml:xml-node-tag node)))
                   (attributes (io.github.cl-sdk.xml:xml-node-attributes node))
                   (children (io.github.cl-sdk.xml:xml-node-children node)))
               (format stream "<~a" tag-name)
               (dolist (attr attributes)
                 (format stream " ~a=\"~a\""
                         (cl-html-input-policies::%xml-name->string (car attr))
                         (cl-html-input-policies::%escape-attribute-value
                          (attribute-value->string (cdr attr)))))
               (if (null children)
                   (write-string "/>" stream)
                   (progn
                     (write-char #\> stream)
                     (dolist (child children)
                       (render-child child stream))
                     (format stream "</~a>" tag-name)))))
           (render-child (child stream)
             (cond
               ((stringp child) (write-string (cl-html-input-policies::%escape-text child) stream))
               ((io.github.cl-sdk.xml:xml-node-p child) (render-node child stream))
               (t (write-string (cl-html-input-policies::%escape-text (princ-to-string child)) stream)))))
    (with-output-to-string (out)
      (dolist (entry fragment)
        (render-child entry out)))))

(test removes-script-tag-and-content
  (let* ((script (make-test-node "script" :children '("alert(1)")))
         (root (make-test-node "p" :children (list "Hello" script "World"))))
    (is (string=
         "<p>HelloWorld</p>"
         (render-sanitized-fragment (sanitize-html-input (make-test-doc root) '("script")))))))

(test removes-denied-tag-but-keeps-its-text-content
  (let* ((b (make-test-node "b" :children '("Hello")))
         (i (make-test-node "i" :children '("world")))
         (root (make-test-node "p" :children (list b " " i))))
    (is (string=
         "<p>Hello world</p>"
         (render-sanitized-fragment (sanitize-html-input (make-test-doc root) '("b" "i")))))))

(test keeps-allowed-tags
  (let* ((em (make-test-node "em" :children '("safe")))
         (root (make-test-node "p" :children (list em " text"))))
    (is (string=
         "<p><em>safe</em> text</p>"
         (render-sanitized-fragment (sanitize-html-input (make-test-doc root) '("script")))))))

(test removes-denied-self-closing-tags
  (let* ((br (make-test-node "br"))
         (root (make-test-node "p" :children (list "Hello" br "World"))))
    (is (string=
         "<p>HelloWorld</p>"
         (render-sanitized-fragment (sanitize-html-input (make-test-doc root) '("br")))))))

(test removes-self-closing-dangerous-tag
  (let* ((script (make-test-node "script"))
         (root (make-test-node "p" :children (list script "safe"))))
    (is (string=
         "<p>safe</p>"
         (render-sanitized-fragment (sanitize-html-input (make-test-doc root) '("script")))))))

(test unwraps-denied-root-while-keeping-children
  (let* ((script (make-test-node "script"))
         (root (make-test-node "root" :children (list "Hello" script "World"))))
    (is (string=
         "HelloWorld"
         (render-sanitized-fragment (sanitize-html-input (make-test-doc root) '("script" "root")))))))

(test strips-denied-root-with-strip-content
  (let ((root (make-test-node "script" :children (list "Hello" "World"))))
    (is (string=
         ""
         (render-sanitized-fragment
          (sanitize-html-input (make-test-doc root)
                               '("script")
                               :strip-content-tags '("script")))))))

(test tag-check-is-case-insensitive
  (let* ((script (make-test-node "SCRIPT" :children '("alert(1)")))
         (root (make-test-node "div" :children (list script))))
    (is (string=
         "<div/>"
         (render-sanitized-fragment (sanitize-html-input (make-test-doc root) '("script")))))))

(test removes-nested-script-tags-with-content
  (let* ((inner (make-test-node "script" :children '("2")))
         (outer (make-test-node "script" :children (list "1" inner "3")))
         (root (make-test-node "p" :children (list outer "x"))))
    (is (string=
         "<p>x</p>"
         (render-sanitized-fragment (sanitize-html-input (make-test-doc root) '("script")))))))

(test removes-script-tags-with-attributes-and-content
  (let* ((script (make-test-node "script"
                                 :attributes '(("src" . "evil.js"))
                                 :children '("alert(1)")))
         (root (make-test-node "p" :children (list script "ok"))))
    (is (string=
         "<p>ok</p>"
         (render-sanitized-fragment (sanitize-html-input (make-test-doc root) '("script")))))))

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
         (render-sanitized-fragment (sanitize-html-input doc '("script")))))))

(test returns-sanitized-fragment-instead-of-string
  (let* ((script (make-test-node "script" :children '("alert(1)")))
         (root (make-test-node "p" :children (list "Hello" script "World")))
         (result (sanitize-html-input (make-test-doc root) '("script")))
         (first-entry (first result)))
    (is (listp result))
    (is (not (stringp result)))
    (is (string= "<p>HelloWorld</p>"
                 (render-sanitized-fragment result)))
    (is (io.github.cl-sdk.xml:xml-node-p first-entry))
    (is (string= "p" (io.github.cl-sdk.xml:xml-node-tag first-entry)))
    (is (equal '("Hello" "World")
               (io.github.cl-sdk.xml:xml-node-children first-entry)))
    (is (every (lambda (entry)
                 (or (stringp entry)
                     (io.github.cl-sdk.xml:xml-node-p entry)))
               result))))
