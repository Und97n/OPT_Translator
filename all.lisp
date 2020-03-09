(uiop/package:define-package :translator/all
    (:use :cl
          :translator/lexer/all
          :translator/common
          :translator/parser/parser
          :translator/tests)
  (:export #:string-lexer
           #:stream-lexer
           #:parser))

(in-package :translator/all)

