(in-package #:cl-html-input-policies)

(defparameter *default-strip-content-tags*
  '("script" "style" "iframe" "object" "embed" "template" "svg" "math")
  "Subset of denied tags whose full element content should be removed.")

(defun %normalize-tag-set (tags)
  (let ((result (make-hash-table :test #'equal)))
    (dolist (tag tags result)
      (setf (gethash (string-downcase (string tag)) result) t))))

(defun %xml-name->string (name)
  (cond
    ((stringp name) name)
    ((symbolp name) (string-downcase (symbol-name name)))
    ((io.github.cl-sdk.xml:xml-qname-p name)
     (let ((prefix (io.github.cl-sdk.xml:xml-qname-prefix name))
           (local-name (io.github.cl-sdk.xml:xml-qname-local-name name)))
       (if prefix
           (format nil "~a:~a" prefix local-name)
           local-name)))
    (t (string-downcase (princ-to-string name)))))

(defun %escape-text (text)
  (with-output-to-string (out)
    (loop for ch across text do
      (case ch
        (#\& (write-string "&amp;" out))
        (#\< (write-string "&lt;" out))
        (#\> (write-string "&gt;" out))
        (t (write-char ch out))))))

(defun %escape-attribute-value (value)
  (with-output-to-string (out)
    (loop for ch across value do
      (case ch
        (#\& (write-string "&amp;" out))
        (#\< (write-string "&lt;" out))
        (#\> (write-string "&gt;" out))
        (#\" (write-string "&quot;" out))
        (t (write-char ch out))))))

(defun %require-string (value context)
  (unless (stringp value)
    (error "Expected ~a to be a string, got ~S" context value))
  value)

(defun sanitize-html-input (input denied-tags &key (strip-content-tags *default-strip-content-tags*))
  "Sanitize parsed HTML/XML INPUT based on DENIED-TAGS.
DENIED-TAGS removes matching tags while keeping their text content.
STRIP-CONTENT-TAGS identifies denied tags whose inner content is also removed and is normally a subset of DENIED-TAGS.
INPUT must be an XML-DOCUMENT or XML-NODE from `io.github.cl-sdk.xml`.
XML comments and processing instructions are removed from the output fragment.
CDATA nodes are converted to plain text entries.
Returns a sanitized fragment (list of strings and XML-NODEs)."
  (let* ((denied (%normalize-tag-set denied-tags))
         (strip-content (%normalize-tag-set strip-content-tags)))
    (labels
        ((drop-from-output-child-node-p (child)
           (or (io.github.cl-sdk.xml:xml-comment-p child)
               (io.github.cl-sdk.xml:xml-pi-p child)))
          (node-policy-state (node)
            (let* ((serialized-tag-name (%xml-name->string (io.github.cl-sdk.xml:xml-node-tag node)))
                   (policy-tag-name (string-downcase serialized-tag-name)))
              (values (gethash policy-tag-name denied)
                      (gethash policy-tag-name strip-content))))
          (node-stripped-with-content-p (node)
            (multiple-value-bind (denied-p strip-p) (node-policy-state node)
              (and denied-p strip-p)))
          (sanitize-children (children)
            (loop for child in children append (sanitize-child child)))
          (sanitize-node (node)
            (multiple-value-bind (denied-p strip-p) (node-policy-state node)
              (let ((children (io.github.cl-sdk.xml:xml-node-children node)))
                (cond
                  ((and denied-p strip-p)
                   nil)
                  (denied-p
                   (sanitize-children children))
                  (t
                   (list
                    (io.github.cl-sdk.xml:make-xml-node
                     :tag (io.github.cl-sdk.xml:xml-node-tag node)
                     :attributes (io.github.cl-sdk.xml:xml-node-attributes node)
                     :children (sanitize-children children))))))))
          (sanitize-child (child)
            (cond
              ((stringp child)
               (list child))
              ((io.github.cl-sdk.xml:xml-node-p child)
               (sanitize-node child))
              ((io.github.cl-sdk.xml:xml-cdata-p child)
               (list (%require-string (io.github.cl-sdk.xml:xml-cdata-data child)
                                      "XML CDATA data")))
              ((drop-from-output-child-node-p child)
               nil)
              (t
               (list (princ-to-string child))))))
      (cond
        ((io.github.cl-sdk.xml:xml-document-p input)
          (let ((root (io.github.cl-sdk.xml:xml-document-root input)))
            (unless (node-stripped-with-content-p root)
              (append (sanitize-children (io.github.cl-sdk.xml:xml-document-prolog input))
                      (sanitize-node root)))))
        ((io.github.cl-sdk.xml:xml-node-p input)
          (sanitize-node input))
        (t
          (error "Unsupported input type ~S. Expected io.github.cl-sdk.xml:XML-DOCUMENT or io.github.cl-sdk.xml:XML-NODE."
                 (type-of input)))))))
