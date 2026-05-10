(in-package #:cl-html-input-policies)

(defparameter *default-strip-content-tags*
  '("script" "style" "iframe" "object" "embed" "template" "svg" "math")
  "Denied tags that should have both tag and inner content removed.")

(defun %normalize-tag-set (tags)
  (let ((result (make-hash-table :test #'equal)))
    (dolist (tag tags result)
      (setf (gethash (string-downcase (string tag)) result) t))))

(defun %xml-parser-function ()
  (let ((pkg (or (find-package "IO.GITHUB.CL-SDK.XML")
                 (find-package :io.github.cl-sdk.xml))))
    (when pkg
      (or (find-symbol "PARSE" pkg)
          (find-symbol "PARSE-STRING" pkg)
          (find-symbol "READ-XML" pkg)
          (find-symbol "FROM-STRING" pkg)
          (find-symbol "LOAD-XML" pkg)))))

(defun %attempt-parse-with-cl-sdk-xml (input)
  (let ((fn-symbol (%xml-parser-function)))
    (when (and fn-symbol (fboundp fn-symbol))
      (handler-case
          (funcall (symbol-function fn-symbol) input)
        (error ()
          nil)))))

(defun %find-tag-end (text start)
  (let* ((len (length text))
         (i start)
         (quote-char nil))
    (loop while (< i len) do
      (let ((ch (char text i)))
        (cond
          (quote-char
           (when (char= ch quote-char)
             (setf quote-char nil)))
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
  "Parse and sanitize HTML/XML-like INPUT by removing denied tags."
  (%attempt-parse-with-cl-sdk-xml input)
  (let* ((denied (%normalize-tag-set denied-tags))
         (strip-content (%normalize-tag-set strip-content-tags))
         (output (make-string-output-stream))
         (len (length input))
         (i 0)
         (blocked-stack '()))
    (flet ((blocked-p () (not (null blocked-stack)))
           (blocked-tag () (car blocked-stack)))
      (loop while (< i len) do
        (let ((ch (char input i)))
          (if (char= ch #\<)
              (let ((end (%find-tag-end input (1+ i))))
                (if (null end)
                    (progn
                      (unless (blocked-p)
                        (write-char ch output))
                      (incf i))
                    (let* ((raw (subseq input (1+ i) end))
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
                        ((or (null tag-name)
                             (not (gethash tag-name denied)))
                         (write-string (subseq input i (1+ end)) output))
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
