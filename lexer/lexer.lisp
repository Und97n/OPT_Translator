(uiop/package:define-package :lexer
    (:use :cl)
  (:export #:stream-lexer))

(defun stream-lexer (stream)
  (labels ((%getc ()
             (read-char stram nil nil))
           (%read-identifier (ch)
             ()))
    ()))
