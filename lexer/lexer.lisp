(uiop/package:define-package :translator/lexer/lexer
    (:use :cl
          :translator/lexer/char-table
          :translator/common
          :translator/utils)
  (:export #:stream-lexer
           #:string-lexer
           #:lexem
           #:lexem-column
           #:lexem-line
           #:lexem-value))

(in-package :translator/lexer/lexer)

(defparameter *keyword-table*
  (hash-table-from-alist
   (list (cons "PROGRAM"
               +keyword-program-id+)
         (cons "BEGIN"
               +keyword-begin-id+)
         (cons "END"
               +keyword-end-id+)
         (cons "PROCEDURE"
               +keyword-procedure-id+)
         (cons "SIGNAL"
               +keyword-signal-id+)
         (cons "COMPLEX"
               +keyword-complex-id+)
         (cons "INTEGER"
               +keyword-integer-id+)
         (cons "FLOAT"
               +keyword-float-id+)
         (cons "BLOCKFLOAT"
               +keyword-blockfloat-id+)
         (cons "EXT"
               +keyword-ext-id+))
   :test #'equal))
(defstruct lexem
  line
  column
  value)

(defun string-lexer (str translator)
  (with-input-from-string (stream str) (stream-lexer stream translator)))

(defun stream-lexer (stream translator)
  (let ((line 1)
        (column 0)
        (current-char)
        (lexems nil))
    (labels ((%current-position ()
               (cons line column))

             (%make-lexem (value &optional (position (%current-position)))
               (make-lexem :line (car position) :column (cdr position)
                           :value value))

             (%getc ()
               (setf current-char (read-char stream nil #\nul))
               (case current-char
                 (#\newline (incf line)
                            (setf column 0))
                 (otherwise (incf column)))
               current-char)

             (%%read-comment (ch)
               (case (get-char-type ch)
                 (:rbracket
                  (%getc)
                  t)
                 (:asterisk
                  (%%read-comment (%getc)))
                 (:eof
                  (error "Lexer error at ~A:~A~%End of file while reading comment"
                         line
                         column))
                 (otherwise
                  (%read-comment (%getc)))))

             (%read-comment (ch)
               (case (get-char-type ch)
                 (:asterisk
                  (%%read-comment (%getc)))
                 (:eof
                  (error "Lexer error at ~A:~A~%End of file while reading comment"
                         line
                         column)
                  (return-from stream-lexer (nreverse lexems)))
                 (otherwise
                  (%read-comment (%getc)))))

             (%read-identifier (ch &optional
                                   (starting-position (%current-position))
                                   identifier-acc)
               (case (get-char-type ch)
                 ((:letter :number)
                  (%read-identifier (%getc)
                                    starting-position
                                    (cons ch identifier-acc)))
                 (otherwise
                  (let ((string (coerce (nreverse identifier-acc)
                                        'string)))
                    (%make-lexem (or (gethash string *keyword-table*)
                                     (ensure-identifier translator string))
                                 starting-position))))))
      (%getc)
      (loop
         :while (not (eq current-char #\nul))
         :do (let ((type (get-char-type current-char)))
               (case type
                 (:letter
                  (push (%read-identifier current-char)
                        lexems))
                 (:space
                  (%getc))
                 (:colon
                  (push (%make-lexem +delimiter-colon-id+)
                        lexems)
                  (%getc))
                 (:semicolon
                  (push (%make-lexem +delimiter-semicolon-id+)
                        lexems)
                  (%getc))
                 (:comma
                  (push (%make-lexem +delimiter-comma-id+)
                        lexems)
                  (%getc))
                 (:rbracket
                  (push (%make-lexem +delimiter-rbracket-id+)
                        lexems)
                  (%getc))
                 (:lbracket
                  (case (get-char-type (%getc))
                    (:asterisk
                     (%read-comment (%getc)))
                    (otherwise
                     (push (%make-lexem +delimiter-lbracket-id+)
                           lexems))))
                 (otherwise
                  (error "Lexer error at ~A:~A~%Unexpected character: ~A"
                         line
                         column
                         current-char)
                  (return-from stream-lexer (nreverse lexems))))))
      (nreverse lexems))))
