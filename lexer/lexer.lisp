(uiop/package:define-package :translator/lexer/lexer
    (:use :cl
          :translator/lexer/char-table)
  (:export #:stream-lexer
           #:string-lexer))

(in-package :translator/lexer/lexer)

(defun string-lexer (str)
  (with-input-from-string (stream str) (stream-lexer stream)))

(defun stream-lexer (stream)
  (let ((line 1)
        (column 0)
        (current-char)
        (lexems nil))
    (labels ((%error (place)
               (error "Lexer error at ~A:~A~%~A: ~A"
                      line
                      column
                      place
                      current-char))
             (%getc ()
               (setf current-char (read-char stream nil #\nul))
               (case current-char
                 (#\newline (incf line)
                            (setf column 1))
                 (otherwise (incf column)))
               current-char)

             (%%read-comment (ch)
               (case (get-char-type ch)
                 ((:rbracket)
                  (%getc)
                  t)
                 ((:eof)
                  (%error "End of file while reading comment"))
                 (otherwise
                  (%read-comment (%getc)))))

             (%read-comment (ch)
               (case (get-char-type ch)
                 ((:asterisk)
                  (%%read-comment (%getc)))
                 ((:eof)
                  (%error "End of file while reading comment"))
                 (otherwise
                  (%read-comment (%getc)))))

             (%read-number (ch &optional number-acc)
               (case (get-char-type ch)
                 ((:number)
                  (%read-number (%getc)
                                (cons ch number-acc)))
                 (otherwise
                  (coerce number-acc 'string))))

             (%read-identifier (ch &optional identifier-acc)
               (case (get-char-type ch)
                 ((:number :letter)
                  (%read-identifier (%getc)
                                    (cons ch identifier-acc)))
                 (otherwise
                  (coerce identifier-acc 'string)))))
      (%getc)
      (loop :while (not (eq current-char #\nul))
         :do (case (get-char-type current-char)
               ((:number)
                (push (%read-number current-char)
                      lexems))
               ((:letter)
                (push (%read-identifier current-char)
                      lexems))
               ((:space)
                (%getc))
               ((:lbracket)
                (case (get-char-type (%getc))
                  ((:asterisk)
                   (%read-comment (%getc)))
                  (otherwise
                   (%error "Unexpected character after '('"))))
               (otherwise
                (%error "Unexpected character"))))
      lexems)))
