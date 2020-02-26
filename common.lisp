(uiop/package:define-package :translator/common
    (:use :cl)
  (:import-from :translator/utils
                #:ensure-gethash)
  (:export #:translator
           #:identifier-table
           #:ensure-identifier
           #:is-identifier-id

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

(defclass translator ()
  ((identifier-table
    :initform (make-hash-table :test #'equal))
   (identifier-id-pointer
    :initform 32768)))

(defun is-identifier-id (id)
  (>= id 32768))

(defun ensure-identifier (tr str)
  (with-slots (identifier-table identifier-id-pointer) tr
    (ensure-gethash str
                    identifier-table
                    (incf identifier-id-pointer))))
