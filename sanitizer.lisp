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
Returns a sanitized string."
  (let* ((denied (%normalize-tag-set denied-tags))
         (strip-content (%normalize-tag-set strip-content-tags))
         (output (make-string-output-stream)))
    (labels
        ((write-attributes (attributes)
           (dolist (attr attributes)
             (let ((name (%xml-name->string (car attr)))
                   (value (cdr attr)))
               (format output " ~a=\"~a\""
                       name
                       (%escape-attribute-value (if value (princ-to-string value) ""))))))
         (write-children (children)
           (dolist (child children)
             (write-child child)))
         (write-node (node)
           (let* ((tag-name (%xml-name->string (io.github.cl-sdk.xml:xml-node-tag node)))
                  (tag-name-down (string-downcase tag-name))
                  (denied-p (gethash tag-name-down denied))
                  (strip-p (gethash tag-name-down strip-content))
                  (children (io.github.cl-sdk.xml:xml-node-children node)))
             (cond
               ((and denied-p strip-p)
                nil)
               (denied-p
                (write-children children))
               (t
                (format output "<~a" tag-name)
                (write-attributes (io.github.cl-sdk.xml:xml-node-attributes node))
                (if (null children)
                    (write-string "/>" output)
                    (progn
                      (write-char #\> output)
                      (write-children children)
                      (format output "</~a>" tag-name)))))))
         (write-child (child)
           (cond
             ((stringp child)
              (write-string (%escape-text child) output))
             ((io.github.cl-sdk.xml:xml-node-p child)
              (write-node child))
             ((io.github.cl-sdk.xml:xml-cdata-p child)
              (write-string (%escape-text (%require-string (io.github.cl-sdk.xml:xml-cdata-data child)
                                                           "XML CDATA data"))
                            output))
             ((io.github.cl-sdk.xml:xml-comment-p child)
              nil)
             ((io.github.cl-sdk.xml:xml-pi-p child)
              nil)
             (t
              (write-string (%escape-text (princ-to-string child)) output)))))
      (cond
        ((io.github.cl-sdk.xml:xml-document-p input)
         (dolist (entry (io.github.cl-sdk.xml:xml-document-prolog input))
           (write-child entry))
         (write-node (io.github.cl-sdk.xml:xml-document-root input)))
        ((io.github.cl-sdk.xml:xml-node-p input)
         (write-node input))
        (t
         (error "Unsupported input type ~S. Expected XML-DOCUMENT or XML-NODE."
                (type-of input)))))
    (get-output-stream-string output)))
