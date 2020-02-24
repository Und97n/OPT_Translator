(uiop/package:define-package :translator/common
    (:use :cl)
  (:import-from :translator/utils
                #:ensure-gethash)
  (:export #:translator
           #:identifier-table
           #:ttt-table
           #:ensure-identifier
           #:ensure-ttt))

(in-package :translator/common)

(defclass translator ()
  ((ttt-table
    :initform (make-hash-table :test #'equal))
   (identifier-table
    :initform (make-hash-table :test #'equal))
   (identifier-id-pointer
    :initform 0)
   (ttt-id-pointer
    :initform 65536)))

(defun ensure-ttt (tr str)
  (with-slots (ttt-table ttt-id-pointer) tr
    (ensure-gethash str
                    ttt-table
                    (incf ttt-id-pointer))))

(defun ensure-identifier (tr str)
  (with-slots (identifier-table identifier-id-pointer) tr
    (ensure-gethash str
                    identifier-table
                    (incf identifier-id-pointer))))
