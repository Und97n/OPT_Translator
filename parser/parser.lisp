(uiop/package:define-package :translator/parser/parser
    (:use :cl
          :translator/common
          :lexer/all
          :translator/utils)
  (:export #:parse))

(in-package :translator/parser/parser)

(defun parse (lexems translator))
