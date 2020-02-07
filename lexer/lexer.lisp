(uiop/package:define-package :translator/lexer/lexer
    (:use :cl
          :translator/lexer/char-table)
  (:export #:stream-lexer
           #:string-lexer))

(in-package :translator/lexer/lexer)

(defun string-lexer (str)
  (with-input-from-string (stream str) (stream-lexer stream)))

(defmacro char-case (char cases &aux (char-type (gensym "CHAR-TYPE")))
  `(let ((,char-type char (get-char-type ,char)))
     (cond
       (()))))

(defun stream-lexer (stream)
  (let ((line 1)
        (column 0)
        (current-char)
        (lexems nil))
    (labels ((%getc ()
               (setf current-char (read-char stream nil nil))
               (if current-char
                   (let ((type (get-char-type current-char)))
                     (if type
                         (etypecase type
                           (newline-char (incf line)
                                         (setf column 1))
                           (t (incf column)))
                         (error "Lexer error at ~A:~A: wrong char: ~A"
                                line
                                column
                                current-char))
                     current-char)
                   nil))
             (%read-number (ch &optional number-acc)
               (etypecase (get-char-type ch)
                 (number-char
                  (%read-number (%getc)
                                (cons ch number-acc)))
                 (t
                  (coerce number-acc 'string))))
             (%read-identifier (ch &optional identifier-acc)
               (etypecase (get-char-type ch)
                 (identifier-char
                  (%read-identifier (%getc)
                                    (cons ch identifier-acc)))
                 (t
                  (coerce identifier-acc 'string)))))
      (%getc)
      (loop :while current-char
         :do (etypecase (get-char-type (%getc))
               (number-char
                (push (%read-number current-char)
                      lexems))
               (identifier-char
                (break "~A: ~A" current-char lexems)
                (push (%read-identifier current-char)
                      lexems))
               (space-char)))
      lexems)))
