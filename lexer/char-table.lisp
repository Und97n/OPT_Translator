(uiop/package:define-package :translator/lexer/char-table
    (:use :cl)
  (:export #:get-char-type
           #:load-char-table))

(in-package :translator/lexer/char-table)

(defparameter *char-table* nil)

(setf *char-table* (make-array 128 :initial-element 'nil))
(labels ((%rcl (type &rest chars)
           (loop :for char :in chars
              :do (setf (aref *char-table* (char-code char))
                        type)))
         (%rcs (type from to)
           (loop :for char :from (char-code from) :to (char-code to)
              :do (setf (aref *char-table* char)
                        type))))
  (%rcs :number #\0 #\9)
  (%rcs :letter #\a #\z)
  (%rcs :letter #\A #\Z)
  (%rcl :eof #\nul)
  (%rcl :semicolon #\;)
  (%rcl :lbracket #\()
  (%rcl :rbracket #\))
  (%rcl :colon #\:)
  (%rcl :comma #\,)
  (%rcl :asterisk #\*)
  (%rcl :space #\  #\newline #\tab))

(defun get-char-type (char)
  (when char
    (let ((code (char-code char)))
      (when (and (>= code 0)
                 (< code (length *char-table*)))
        (aref *char-table* code)))))
