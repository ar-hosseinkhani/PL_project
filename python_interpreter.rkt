#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define python-lexer
  (lexer
   (whitespace (python-lexer input-port))
   ((eof) (token-EOF))
   ("pass" (token-pass))
   ("continue" (token-continue))
   ("break" (token-break))
   ("return" (token-return))
   ("global" (token-global))
   ("def" (token-def))
   ("if" (token-if))
   ("else" (token-else))
   ("for" (token-for))
   ("in" (token-in))
   ("or" (token-or))
   ("and" (token-and))
   ("not" (token-not))
   ("True" (token-True))
   ("False" (token-False))
   ("None" (token-None))
   ((:: (:or (char-range #\a #\z)
             (char-range #\A #\Z))
        (:* (:or (char-range #\a #\z)
                 (char-range #\A #\Z)
                 (char-range #\0 #\9)
                 "_"))) (token-ID lexeme))))

(define-tokens a (ID))
(define-empty-tokens b (EOF pass continue break return global def if else for in or and not True False None))


(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this python-lexer (open-input-string "pass return break def else for False None ali s23_a")))