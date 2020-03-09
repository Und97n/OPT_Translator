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

(defun print-tree (syntax-tree translator)
  (labels ((%process-element (x spaces)
             (cond
               ((numberp x)
                (format t "~%~{~A~}[~A:~A]" spaces x (id-to-string x nil translator)))
               ((listp x)
                (format t "~%~{~A~}<~A>" spaces (first x))
                (mapc (lambda (x)
                        (%process-element x (cons ".." spaces)))
                      (rest x)))
               (t
                (format t "~%~{~A~}~A" spaces x)))))
    (%process-element syntax-tree nil)))

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
      (format t "TREE:~%")
      (print-tree tree translator)
      (with-slots (identifier-table ttt-table) translator
        (format t "~%IDENTIFIERS:~%~:{~4T~7A -- ~2A~%~}"
                (mapcar (lambda (c)
                          (list (car c) (cdr c)))
                        (hash-table-to-alist identifier-table)))))))
