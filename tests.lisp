(uiop/package:define-package :translator/tests
  (:use :cl
        :translator/lexer/lexer
        :translator/parser/parser
        :translator/common
        :translator/utils)
  (:export #:run-test
           #:run-tests))

(in-package :translator/tests)

(defun run-tests (tests-dir)
  (mapc #'run-test (directory (format nil "~A*" tests-dir))))

(defun print-tree (stream syntax-tree translator)
  (labels ((%process-element (x spaces)
             (cond
               ((numberp x)
                (format stream "~%~{~A~}[~A:~A]" spaces x (id-to-string x translator)))
               ((listp x)
                (format stream "~%~{~A~}<~A>" spaces (first x))
                (mapc (lambda (x)
                        (%process-element x (cons ".." spaces)))
                      (rest x)))
               (t
                (format stream "~%~{~A~}~A" spaces x)))))
    (%process-element syntax-tree nil)))

(defun run-test (test-path)
  (let ((generated-f (format nil "~Agenerated.sig" test-path))
        (input-f (format nil "~Ainput.sig" test-path)))
    (format t "~2%TESTING ~A:~%" test-path)
    (when (probe-file generated-f)
      (delete-file generated-f))
    (with-open-file (stream input-f)
      (with-open-file (generated
                       generated-f
                       :direction :output
                       :if-does-not-exist :create)
        (let* ((translator (make-instance 'translator))
               (lexems (stream-lexer stream translator))
               (tree (parser lexems translator)))
          (format generated "LEXEMS:~%~:{~4T~2A: ~2A - ~A (~A)~%~}"
                  (mapcar (lambda (x)
                            (list (lexem-line x)
                                  (lexem-column x)
                                  (id-to-string (lexem-value x) translator)
                                  (lexem-value x)))
                          lexems))
          (format generated "TREE:~%")
          (print-tree generated tree translator)
          (with-slots (identifier-table ttt-table) translator
            (format generated "~%IDENTIFIERS:~%~:{~4T~7A -- ~2A~%~}"
                    (mapcar (lambda (c)
                              (list (car c) (cdr c)))
                            (hash-table-to-alist identifier-table)))))))))
