(uiop/package:define-package :translator/parser/parser
    (:use :cl
          :translator/common
          :translator/lexer/all
          :translator/utils)
  (:import-from :translator/lexer/lexer
                #:lexem-column
                #:lexem-line
                #:lexem-value)
  (:export #:parser))

(in-package :translator/parser/parser)

(defun parser (lexems translator)
  (let ((current-lexem nil))
    (labels ((%require-lexem (lexem test expected)
               (unless lexem
                 (error "Parser error: ~A expected, EOF found"
                        expected))
               (if (funcall test (lexem-value lexem))
                   (lexem-value lexem)
                   (error "Parser error at ~A:~A~%~A expected, ~A found"
                          (lexem-line lexem)
                          (lexem-column lexem)
                          expected
                          (id-to-string (lexem-value lexem)
                                        nil
                                        translator))))
             (%identifier ()
               (prog1 (list :identifier (%require-lexem current-lexem #'is-identifier-id "identifier"))
                 (%getl)))

             (%attribute ()
               (prog1 (%require-lexem current-lexem #'is-attribute-id "attribute")
                 (%getl)))

             (%require-lexem-id (id expected)
               (prog1 (%require-lexem current-lexem (lambda (x) (= x id)) expected)
                 (%getl)))

             (%getl ()
               (setf current-lexem (pop lexems))
               (pprint current-lexem)
               current-lexem)

             (%identifiers-list ()
               (cons :identifiers-list
                     (loop :collect (%identifier)
                        :while (when (and current-lexem
                                          (= (lexem-value current-lexem) +delimiter-comma-id+))
                                 (%getl)))))

             (%attributes-list ()
               (cons :attributes-list
                     (loop :collect (%attribute)
                        :while (and current-lexem
                                    (is-attribute-id (lexem-value current-lexem))))))

             (%declaration ()
               (let ((inds (%identifiers-list)))
                 (%require-lexem-id +delimiter-colon-id+ "colon")
                 (let ((attrs (%attributes-list)))
                   (%require-lexem-id +delimiter-semicolon-id+ "semicolon")
                   (list :declaration inds attrs))))

             (%parameters-list ()
               (cons :parameters-list
                     (when (= (lexem-value current-lexem)
                              +delimiter-lbracket-id+)
                       (prog2
                           (%require-lexem-id +delimiter-lbracket-id+ "lbracket")
                           (loop :while (and current-lexem
                                             (is-identifier-id (lexem-value current-lexem)))
                              :collect (%declaration))
                         (%require-lexem-id +delimiter-rbracket-id+ "rbracket")))))

             (%procedure ()
               (%require-lexem-id +keyword-procedure-id+ "PROCEDURE")
               (let ((name (%identifier))
                     (plist (%parameters-list)))
                 (%require-lexem-id +delimiter-semicolon-id+ "semicolon")
                 (list :procedure name plist)))

             (%statements-list ()
               (list :statements-list nil))

             (%declarations ()
               (cons :declarations
                     (loop :while (and current-lexem
                                       (= (lexem-value current-lexem)
                                          +keyword-procedure-id+))
                        :collect (%procedure))))

             (%block ()
               (let ((stl (%statements-list))
                     (decls (%declarations)))
                 (%require-lexem-id +keyword-begin-id+ "BEGIN")
                 (%require-lexem-id +keyword-end-id+ "END")
                 (list :block stl decls)))

             (%program ()
               (%require-lexem-id +keyword-program-id+ "PROGRAM")
               (let ((pname (%identifier)))
                 (%require-lexem-id +delimiter-semicolon-id+ "semicolon")
                 (let ((body (%block)))
                   (%require-lexem-id +delimiter-semicolon-id+ "semicolon")
                   (list :program pname body)))))
      (%getl)
      (%program))))
