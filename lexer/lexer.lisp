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

(defparameter *keyword-table* (hash-table-from-alist
                               '(("PROGRAM" . :program)
                                 ("BEGIN" . :begin)
                                 ("END" . :end)
                                 ("PROCEDURE" . :procedure)
                                 ("SIGNAL" . :signal)
                                 ("COMPLEX" . :complex)
                                 ("INTEGER" . :integer)
                                 ("FLOAT" . :float)
                                 ("BLOCKFLOAT" . :blockfloat)
                                 ("EXT" . :ext))
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

             (%error (place)
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
                 ((:asterisk)
                  (%%read-comment (%getc)))
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

             (%read-identifier (ch &optional
                                   (starting-position (%current-position))
                                   identifier-acc)
               (case (get-char-type ch)
                 ((:comma :semicolon :colon :eof :space :lbracket :rbracket)
                  (let ((string (coerce (nreverse identifier-acc)
                                        'string)))
                    (%make-lexem (or (gethash string *keyword-table*)
                                     (ensure-identifier translator string))
                                 starting-position)))
                 ((:letter :number)
                  (%read-identifier (%getc)
                                    starting-position
                                    (cons ch identifier-acc)))
                 (otherwise
                  (%error "Wron char while reading identifier")))))
      (%getc)
      (loop
         :while (not (eq current-char #\nul))
         :do (let ((type (get-char-type current-char)))
               (case type
                 ((:letter)
                  (push (%read-identifier current-char)
                        lexems))
                 ((:space)
                  (%getc))
                 ((:colon :semicolon :comma :rbracket)
                  (push (%make-lexem type)
                        lexems)
                  (%getc))
                 ((:lbracket)
                  (case (get-char-type (%getc))
                    ((:asterisk)
                     (%read-comment (%getc)))
                    (otherwise
                     (push (%make-lexem :lbracket)
                           lexems))))
                 (otherwise
                  (%error "Unexpected character")))))
      (nreverse lexems))))
