(uiop/package:define-package :translator/lexer/char-table
    (:use :cl)
  (:export #:get-char-type
           #:char-type
           #:define-char-type
           #:load-char-table))

(in-package :translator/lexer/char-table)

(defclass char-type () ())

(defparameter *char-initializers* nil)

(defmacro define-char-type (name supertypes &optional code to-code)
  (when code
    (push (list name code (or to-code code))
          *char-initializers*))
  `(progn ,(if supertypes
               `(defclass ,name (,@supertypes) ())
               `(defclass ,name (char-type) ()))
          ;; (defparameter ,name (make-instance (quote ,name)))
          ))

(defparameter +char-table+ (make-array 128 :initial-element 'nil))

(defun get-char-type (char)
  (when char
    (let ((code (char-code char)))
      (when (and (> code 0)
                 (< code (length +char-table+)))
        (aref +char-table+ code)))))

(defun load-char-table ()
  (labels ((%rcs (type from to)
             (loop :for char :from (char-code from) :to (char-code to)
                :do (setf (aref +char-table+ char)
                          (make-instance type)))))
    (loop :for initializer :in *char-initializers*
       :do (apply #'%rcs initializer))))
