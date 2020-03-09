(uiop/package:define-package :translator/tests
    (:use :cl
          :translator/lexer/lexer
          :translator/parser/parser
          :translator/common
          :translator/utils)
  (:export #:run-test
           #:run-tests))

(in-package :translator/tests)

(defun run-tests (&rest paths)
  (mapc #'run-test paths))

(defun tree-to-pretty-form (syntax-tree translator)
  (labels ((%process-element (x)
             (cond
               ((numberp x)
                (id-to-string x nil translator))
               ((listp x)
                (mapcar #'%process-element x))
               (t
                x))))
    (%process-element syntax-tree)))

(defun run-test (test-path)
  (format t "~2%TESTING ~A:~%" test-path)
  (with-open-file (stream test-path)
    (let* ((translator (make-instance 'translator))
           (lexems (stream-lexer stream translator))
           (tree (parser lexems translator)))
      (format t "LEXEMS:~%~:{~4T~2A: ~2A - ~A~%~}"
              (mapcar (lambda (x)
                        (list (lexem-line x)
                              (lexem-column x)
                              (lexem-value x)))
                      lexems))
      (format t "TREE:~%~4T~A~%" (tree-to-pretty-form tree translator))
      (with-slots (identifier-table ttt-table) translator
        (format t "~%IDENTIFIERS:~%~:{~4T~7A -- ~2A~%~}"
                (mapcar (lambda (c)
                          (list (car c) (cdr c)))
                        (hash-table-to-alist identifier-table)))))))
