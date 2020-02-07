(uiop/package:define-package :lexer/char-table
    (:use :lexer/all)
  (:export #:get-char-type))

(defconstant +char-table+ (make-array 128 :initia-element 'nil))

(defun get-char-type (char)
  (let ((code (char-code char)))
    (when (and (> code 0)
               (< code (length +char-table+)))
      (aref +char-table+ code))))

(defun load-char-table ()
  (labels ((%rc (char type)
             (setf (aref +char-table+ (char-code char))))
           (%rcs (from to type)
             (loop :for index :from from :to to
                :do (%rc index type))))

    (%rc #\  :space)
    (%rc #\newline :space)
    (%rc #\t :space)
    (%rcs #\a #\z :letter)
    (%rcs #\A #\Z :letter)
    (%rcs #\0 #\9 :number)))
