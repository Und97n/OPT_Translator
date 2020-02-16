(uiop/package:define-package :translator/utils
    (:use :cl)
  (:export #:aif
           #:ensure-gethash
           #:hash-table-from-alist
           #:hash-table-to-alist))

(in-package :translator/utils)

(defmacro aif (expr true-case false-case &aux (tmp (gensym)))
  `(let ((,tmp ,expr))
     (if ,tmp
         (let ((it ,tmp))
           ,true-case)
         ,false-case)))

(defmacro ensure-gethash (key hash-table &optional default)
  (let ((key-tmp (gensym))
        (ht-tmp (gensym))
        (val-tmp (gensym))
        (presentp-tmp (gensym)))
    `(let ((,key-tmp ,key)
           (,ht-tmp ,hash-table))
       (multiple-value-bind (,val-tmp ,presentp-tmp) (gethash ,key-tmp ,ht-tmp)
         (if ,presentp-tmp
             (values ,val-tmp t)
             (values (setf (gethash ,key-tmp ,ht-tmp) ,default) nil))))))

(defun hash-table-from-alist (alist &rest hash-table-initargs)
  (let ((table (apply #'make-hash-table hash-table-initargs)))
    (dolist (cons alist)
      (ensure-gethash (car cons) table (cdr cons)))
    table))

(defun hash-table-to-alist (ht)
  (let ((lst nil))
    (maphash (lambda (k v)
               (push (cons k v) lst))
             ht)
    lst))
