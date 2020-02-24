(uiop/package:define-package :translator/lexer/tests
    (:use :cl
          :translator/lexer/lexer
          :translator/common
          :translator/utils)
  (:export #:run-test
           #:run-tests))

(in-package :translator/lexer/tests)

(defun run-tests (&rest paths)
  (mapc #'run-test paths))

(defun run-test (test-path)
  (format t "~2%TESTING ~A:~%" test-path)
  (with-open-file (stream test-path)
    (let* ((translator (make-instance 'translator))
           (lexems (stream-lexer stream translator)))
      (format t "LEXEMS:~%~:{~4T~2A: ~2A - ~A~%~}" (mapcar (lambda (x)
                                                             (list (lexem-line x)
                                                                   (lexem-column x)
                                                                   (lexem-value x)))
                                                           lexems))
      (with-slots (identifier-table ttt-table) translator
        (format t "~%IDENTIFIERS:~%~:{~4T~7A -- ~2A~%~}" (mapcar (lambda (c)
                                                                   (list (car c) (cdr c)))
                                                                 (hash-table-to-alist identifier-table)))
        (format t "~%TTT:~%~:{~4T~7A -- ~2A~%~}" (mapcar (lambda (c)
                                                           (list (car c) (cdr c)))
                                                         (hash-table-to-alist ttt-table))))))
  )
