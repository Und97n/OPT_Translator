(uiop/package:define-package :translator/common
    (:use :cl)
  (:import-from :translator/utils
                #:ensure-gethash)
  (:export #:translator
           #:identifier-table
           #:ensure-identifier))

(in-package :translator/common)

(defclass translator ()
  ((identifier-table
    :initform (make-hash-table :test #'eq))
   (identifier-id-pointer
    :initform 0)))

(defmethod ensure-identifier (tr str)
  (with-slots (identifier-table identifier-id-pointer) tr
    (ensure-gethash str
                    identifier-table
                    (incf identifier-id-pointer))))
