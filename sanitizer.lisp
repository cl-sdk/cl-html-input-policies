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

(defun %serialize-xml-attributes (attributes)
  (with-output-to-string (out)
    (dolist (attr attributes)
      (let ((name (%xml-name->string (car attr)))
            (value (cdr attr)))
        (format out " ~a=\"~a\""
                name
                (%escape-attribute-value (if value (princ-to-string value) "")))))))

(defun %ensure-no-sequence (text sequence context)
  (when (search sequence text)
    (error "Unsafe ~a content contains forbidden sequence ~s: ~s"
           context
           sequence
           text))
  text)

(defun %require-string (value context)
  (unless (stringp value)
    (error "Expected ~a to be a string, got ~S" context value))
  value)

(defun %serialize-xml-child (child)
  (cond
    ((stringp child) (%escape-text child))
    ((io.github.cl-sdk.xml:xml-node-p child) (%serialize-xml-node child))
    ((io.github.cl-sdk.xml:xml-comment-p child)
     (let ((data (%ensure-no-sequence (%require-string (io.github.cl-sdk.xml:xml-comment-data child)
                                                       "XML comment data")
                                      "-->"
                                      "XML comment")))
       (format nil "<!--~a-->" data)))
    ((io.github.cl-sdk.xml:xml-cdata-p child)
     (let ((data (%ensure-no-sequence (%require-string (io.github.cl-sdk.xml:xml-cdata-data child)
                                                       "XML CDATA data")
                                      "]]>"
                                      "XML CDATA")))
       (format nil "<![CDATA[~a]]>" data)))
    ((io.github.cl-sdk.xml:xml-pi-p child)
     (let ((target (%ensure-no-sequence (%require-string (io.github.cl-sdk.xml:xml-pi-target child)
                                                         "XML processing-instruction target")
                                        "?>"
                                        "XML processing-instruction target"))
           (data (%ensure-no-sequence (%require-string (io.github.cl-sdk.xml:xml-pi-data child)
                                                       "XML processing-instruction data")
                                      "?>"
                                      "XML processing-instruction data")))
       (format nil "<?~a ~a?>" target data)))
    (t (%escape-text (princ-to-string child)))))

(defun %serialize-xml-node (node)
  (let* ((tag (%xml-name->string (io.github.cl-sdk.xml:xml-node-tag node)))
         (attributes (%serialize-xml-attributes (io.github.cl-sdk.xml:xml-node-attributes node)))
         (children (io.github.cl-sdk.xml:xml-node-children node)))
    (if (null children)
        (format nil "<~a~a/>" tag attributes)
        (with-output-to-string (out)
          (format out "<~a~a>" tag attributes)
          (dolist (child children)
            (write-string (%serialize-xml-child child) out))
          (format out "</~a>" tag)))))

(defun %input->string (input)
  (cond
    ((stringp input) input)
    ((io.github.cl-sdk.xml:xml-document-p input)
     (with-output-to-string (out)
       (dolist (entry (io.github.cl-sdk.xml:xml-document-prolog input))
         (write-string (%serialize-xml-child entry) out))
       (write-string (%serialize-xml-node (io.github.cl-sdk.xml:xml-document-root input)) out)))
    ((io.github.cl-sdk.xml:xml-node-p input)
     (%serialize-xml-node input))
    (t (error "Unsupported input type ~S. Expected string, XML-DOCUMENT, or XML-NODE." (type-of input)))))

(defun %find-tag-end (text start)
  (let* ((len (length text))
         (i start)
         (quote-char nil)
         (escaped-p nil))
    (loop while (< i len) do
      (let ((ch (char text i)))
        (cond
          (quote-char
           (cond
             (escaped-p
              (setf escaped-p nil))
             ((char= ch #\\)
              (setf escaped-p t))
             ((char= ch quote-char)
              (setf quote-char nil))))
          ((or (char= ch #\") (char= ch #\'))
           (setf quote-char ch))
          ((char= ch #\>)
           (return i))))
      (incf i))
    nil))

(defun %extract-tag-name (raw-tag)
  (let* ((len (length raw-tag))
         (i 0))
    (when (and (> len 0) (char= (char raw-tag i) #\/))
      (incf i))
    (loop while (and (< i len)
                     (find (char raw-tag i) " \t\n\r")) do
      (incf i))
    (let ((start i))
      (loop while (and (< i len)
                       (or (alphanumericp (char raw-tag i))
                           (find (char raw-tag i) "_:-"))) do
        (incf i))
      (if (> i start)
          (string-downcase (subseq raw-tag start i))
          nil))))

(defun %closing-tag-p (raw-tag)
  (let ((len (length raw-tag))
        (i 0))
    (loop while (and (< i len)
                     (find (char raw-tag i) " \t\n\r")) do
      (incf i))
    (and (< i len) (char= (char raw-tag i) #\/))))

(defun %self-closing-tag-p (raw-tag)
  (let ((i (1- (length raw-tag))))
    (loop while (and (>= i 0)
                     (find (char raw-tag i) " \t\n\r")) do
      (decf i))
    (and (>= i 0) (char= (char raw-tag i) #\/))))

(defun sanitize-html-input (input denied-tags &key (strip-content-tags *default-strip-content-tags*))
  "Sanitize HTML/XML-like INPUT based on DENIED-TAGS.
DENIED-TAGS removes matching tags while keeping their text content.
STRIP-CONTENT-TAGS identifies denied tags whose inner content is also removed and is normally a subset of DENIED-TAGS.
INPUT may be a string, XML-DOCUMENT, or XML-NODE from `io.github.cl-sdk.xml`.
Returns a sanitized string."
  (let* ((input-text (%input->string input))
         (denied (%normalize-tag-set denied-tags))
         (strip-content (%normalize-tag-set strip-content-tags))
         (output (make-string-output-stream))
         (len (length input-text))
         (i 0)
         (blocked-stack '()))
    (flet ((blocked-p () (not (null blocked-stack)))
           (blocked-tag () (car blocked-stack)))
      (loop while (< i len) do
        (let ((ch (char input-text i)))
          (if (char= ch #\<)
              (let ((end (%find-tag-end input-text (1+ i))))
                (if (null end)
                    (progn
                      (unless (blocked-p)
                        (write-char ch output))
                      (incf i))
                    (let* ((raw (subseq input-text (1+ i) end))
                           (tag-name (%extract-tag-name raw))
                           (closing-p (%closing-tag-p raw))
                           (self-closing-p (%self-closing-tag-p raw)))
                      (cond
                        ((blocked-p)
                         (when tag-name
                           (cond
                             ((and closing-p (string= tag-name (blocked-tag)))
                              (pop blocked-stack))
                             ((and (not closing-p)
                                   (gethash tag-name strip-content)
                                   (not self-closing-p))
                              (push tag-name blocked-stack)))))
                        ((null tag-name))
                         ((not (gethash tag-name denied))
                          (write-string (subseq input-text i (1+ end)) output))
                        ((and (gethash tag-name strip-content)
                              (not closing-p)
                              (not self-closing-p))
                         (push tag-name blocked-stack)))
                      (setf i (1+ end)))))
              (progn
                (unless (blocked-p)
                  (write-char ch output))
                (incf i))))))
    (get-output-stream-string output)))
