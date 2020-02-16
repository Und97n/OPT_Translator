(uiop/package:define-package :translator/all
    (:use :cl
          :translator/lexer/all
          :translator/common)
  (:export #:string-lexer
           #:stream-lexer))

(in-package :translator/all)

