(uiop/package:define-package :translator/common
    (:use :cl)
  (:import-from :translator/utils
                #:ensure-gethash)
  (:export #:translator
           #:identifier-table
           #:identifier-vector
           #:finish-lexer
           #:id-to-string
           #:ensure-identifier
           #:is-identifier-id
           #:is-attribute-id

           #:+delimiter-colon-id+
           #:+delimiter-semicolon-id+
           #:+delimiter-comma-id+
           #:+delimiter-lbracket-id+
           #:+delimiter-rbracket-id+

           #:+keyword-program-id+
           #:+keyword-begin-id+
           #:+keyword-end-id+
           #:+keyword-procedure-id+
           #:+keyword-signal-id+
           #:+keyword-complex-id+
           #:+keyword-integer-id+
           #:+keyword-float-id+
           #:+keyword-blockfloat-id+
           #:+keyword-ext-id+))

(in-package :translator/common)

(defconstant +delimiter-colon-id+ 128)
(defconstant +delimiter-semicolon-id+ 129)
(defconstant +delimiter-comma-id+ 130)
(defconstant +delimiter-lbracket-id+ 131)
(defconstant +delimiter-rbracket-id+ 132)

(defconstant +keyword-program-id+ 256)
(defconstant +keyword-begin-id+ 257)
(defconstant +keyword-end-id+ 258)
(defconstant +keyword-procedure-id+ 259)

(defconstant +keyword-signal-id+ 260)
(defconstant +keyword-complex-id+ 261)
(defconstant +keyword-integer-id+ 262)
(defconstant +keyword-float-id+ 263)
(defconstant +keyword-blockfloat-id+ 263)
(defconstant +keyword-ext-id+ 263)

(defconstant +identifier-ids-start+ 32768)

(defclass translator ()
  ((identifier-table
    :initform (make-hash-table :test #'equal))
   (identifier-vector)
   (identifier-id-pointer
    :initform +identifier-ids-start+)))

(defun is-identifier-id (id)
  (>= id +identifier-ids-start+))

(defun is-attribute-id (id)
  (and (>= id 260)
       (<= id 263)))

(defun id-to-string (id stream translator)
  (format stream "~A"
          (cond
            ((= id +delimiter-colon-id+) ":")
            ((= id +delimiter-semicolon-id+) ";")
            ((= id +delimiter-comma-id+) ",")
            ((= id +delimiter-lbracket-id+) "(")
            ((= id +delimiter-rbracket-id+) ")")

            ((= id +keyword-program-id+) "PROGRAM")
            ((= id +keyword-begin-id+) "BEGIN")
            ((= id +keyword-end-id+) "END")
            ((= id +keyword-procedure-id+) "PROCEDURE")

            ((= id +keyword-signal-id+) "SIGNAL")
            ((= id +keyword-complex-id+) "COMPLEX")
            ((= id +keyword-integer-id+) "INTEGER")
            ((= id +keyword-float-id+) "FLOAT")
            ((= id +keyword-blockfloat-id+) "BLOCKFLOAT")
            ((= id +keyword-ext-id+) "EXT")
            (t
             (if (is-identifier-id id)
                 (with-slots (identifier-vector) translator
                   (aref identifier-vector (- id +identifier-ids-start+ 1)))
                 "???")))))

(defun finish-lexer (translator)
  (with-slots (identifier-table identifier-vector) translator
    (setf identifier-vector
          (make-array (hash-table-count identifier-table)))
    (maphash (lambda (ind id)
               (setf (aref identifier-vector (- id +identifier-ids-start+ 1)) ind))
             identifier-table)))

(defun ensure-identifier (tr str)
  (with-slots (identifier-table identifier-id-pointer) tr
    (ensure-gethash str
                    identifier-table
                    (incf identifier-id-pointer))))
