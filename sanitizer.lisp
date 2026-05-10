(in-package #:io.github.cl-sdk.html-input-policies)

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

(defparameter +xml-whitespace-chars+ (list #\Space #\Tab #\Newline #\Return))

(defun %whitespace-only-text-p (text)
  (every (lambda (ch) (find ch +xml-whitespace-chars+)) text))

(defclass sanitizing-dom-builder (io.github.cl-sdk.xml:dom-builder)
  ((%denied-tags :initarg :denied-tags :reader sanitizing-dom-builder-denied-tags)
   (%strip-content-tags :initarg :strip-content-tags :reader sanitizing-dom-builder-strip-content-tags)
   (%strip-depth :initform 0 :accessor sanitizing-dom-builder-strip-depth)
   (%stack :initform nil :accessor sanitizing-dom-builder-stack)
   (%fragment :initform nil :accessor sanitizing-dom-builder-fragment)))

(defun %tag-policy-state (tag denied strip-content)
  (let* ((policy-tag-name (string-downcase (%xml-name->string tag)))
         (denied-p (gethash policy-tag-name denied)))
    (values denied-p
            (and denied-p (gethash policy-tag-name strip-content)))))

(defun %builder-policy-state (handler tag)
  (%tag-policy-state tag
                     (sanitizing-dom-builder-denied-tags handler)
                     (sanitizing-dom-builder-strip-content-tags handler)))

(defun %builder-push-fragment-child (handler child)
  (if (sanitizing-dom-builder-stack handler)
      (let ((frame (first (sanitizing-dom-builder-stack handler))))
        (push child (third frame)))
      (push child (sanitizing-dom-builder-fragment handler))))

(defmethod io.github.cl-sdk.xml:start-document ((handler sanitizing-dom-builder))
  (setf (sanitizing-dom-builder-strip-depth handler) 0
        (sanitizing-dom-builder-stack handler) nil
        (sanitizing-dom-builder-fragment handler) nil))

(defmethod io.github.cl-sdk.xml:start-element ((handler sanitizing-dom-builder) tag attributes)
  (cond
    ((plusp (sanitizing-dom-builder-strip-depth handler))
     (incf (sanitizing-dom-builder-strip-depth handler)))
    (t
     (multiple-value-bind (denied-p strip-p) (%builder-policy-state handler tag)
       (cond
         ((and denied-p strip-p)
          (setf (sanitizing-dom-builder-strip-depth handler) 1))
         (denied-p
          nil)
         (t
          (push (list tag attributes nil)
                (sanitizing-dom-builder-stack handler))))))))

(defmethod io.github.cl-sdk.xml:end-element ((handler sanitizing-dom-builder) tag)
  (cond
    ((plusp (sanitizing-dom-builder-strip-depth handler))
     (decf (sanitizing-dom-builder-strip-depth handler)))
    (t
     (multiple-value-bind (denied-p strip-p) (%builder-policy-state handler tag)
       (declare (ignore strip-p))
       (unless denied-p
         (let* ((frame (pop (sanitizing-dom-builder-stack handler)))
                (node  (io.github.cl-sdk.xml:make-xml-node
                        :tag (first frame)
                        :attributes (second frame)
                        :children (nreverse (third frame)))))
           (%builder-push-fragment-child handler node)))))))

(defmethod io.github.cl-sdk.xml:characters ((handler sanitizing-dom-builder) text)
  (unless (or (plusp (sanitizing-dom-builder-strip-depth handler))
              (%whitespace-only-text-p text))
    (%builder-push-fragment-child handler text)))

(defmethod io.github.cl-sdk.xml:comment ((handler sanitizing-dom-builder) data)
  (declare (ignore handler data))
  nil)

(defmethod io.github.cl-sdk.xml:processing-instruction ((handler sanitizing-dom-builder) target data)
  (declare (ignore handler target data))
  nil)

(defmethod io.github.cl-sdk.xml:cdata-section ((handler sanitizing-dom-builder) data)
  (io.github.cl-sdk.xml:characters handler (%require-string data "CDATA section data")))

(defmethod io.github.cl-sdk.xml:doctype-declaration ((handler sanitizing-dom-builder) doctype)
  (declare (ignore handler doctype))
  nil)

(defmethod io.github.cl-sdk.xml:end-document ((handler sanitizing-dom-builder))
  (nreverse (sanitizing-dom-builder-fragment handler)))

(defun parse-and-sanitize-html-input (input denied-tags &key (strip-content-tags *default-strip-content-tags*))
  "Parse INPUT and sanitize during DOM building using this package's local DOM-BUILDER extension.
DENIED-TAGS is a list of denied tag names (strings/symbols).
STRIP-CONTENT-TAGS is the subset of denied tags whose full content is removed.
Returns a sanitized fragment (list of strings and XML-NODEs)."
  (let ((handler (make-instance 'sanitizing-dom-builder
                                :denied-tags (%normalize-tag-set denied-tags)
                                :strip-content-tags (%normalize-tag-set strip-content-tags))))
    (io.github.cl-sdk.xml:parse-xml input :handler handler)))

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
            (let ((policy-tag-name (string-downcase
                                    (%xml-name->string (io.github.cl-sdk.xml:xml-node-tag node)))))
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
