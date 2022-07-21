#lang racket

(require (lib "eopl.ss" "eopl"))

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
   ("==" (token-equasion))
   (">" (token-grater-than))
   ("<" (token-less-than))
   ("+" (token-plus))
   ("-" (token-minus))
   ("**" (token-power))
   ("/" (token-division))
   ("*" (token-mult))
   ("[" (token-bracket-begin))
   ("]" (token-bracket-end))
   (":" (token-colon))
   ("(" (token-parenthes-begin))
   (")" (token-parenthes-end))
   ("," (token-comma))
   ("=" (token-assignment))
   (";" (token-semicolon))
   ((:or (:+ (char-range #\0 #\9))
         (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9))))
    (token-NUM (string->number lexeme)))
   ((:: (:or (char-range #\a #\z)
             (char-range #\A #\Z))
        (:* (:or (char-range #\a #\z)
                 (char-range #\A #\Z)
                 (char-range #\0 #\9)
                 "_"))) (token-ID lexeme))))
(define-tokens a (ID NUM))
(define-empty-tokens b (EOF pass continue break return global def if else for in or and not True False
                            None equasion grater-than less-than plus minus power division mult
                            bracket-begin bracket-end colon parenthes-begin parenthes-end comma
                            assignment semicolon))

(define python-parser
  (parser
   (start program)
   (end EOF)
   (error void)
   (tokens a b)
   (grammar
    (program ((statements) (list 'program $1)))
    (statements ((statement semicolon) $1)
                ((statements statement semicolon) (list 'statements $1 $2)))
    (statement ((compound_stmt) $1)
               ((simple_stmt) $1))
    (simple_stmt ((assignment_stmt) $1)
                 ((global_stmt) $1)
                 ((return_stmt) $1)
                 ((pass) (list 'pass))
                 ((break) (list 'break))
                 ((continue) (list 'continue)))
    (compound_stmt ((function_def) $1)
                   ((if_stmt) $1)
                   ((for_stmt) $1))
    (assignment_stmt ((ID assignment expression) (list 'assignment $1 $3)))
    (return_stmt ((return) (list 'return-non))
                 ((return expression) (list 'return $2)))
    (global_stmt ((global ID) (list 'global $2)))
    (function_def ((def ID parenthes-begin params parenthes-end colon statements) (list 'func_def $2 $4 $7))
                  ((def ID parenthes-begin parenthes-end colon statements) (list 'func_def_no_param $2 $6)))
    (params ((param_with_default) $1)
            ((params comma param_with_default) (list 'params $1 $3)))
    (param_with_default ((ID assignment expression) (list 'param $1 $3)))
    (if_stmt ((if expression colon statements else_block) (list 'if $2 $4 $5)))
    (else_block ((else colon statements) (list 'else $3)))
    (for_stmt ((for ID in expression colon statements) (list 'for $2 $4 $6)))
    (expression ((disjunction) $1))
    (disjunction ((conjunction) $1)
                 ((disjunction or conjunction) (list 'disjunction $1 $3))) ; TODO: check
    (conjunction ((inversion) $1)
                 ((conjunction and inversion) (list 'conjunction $1 $3)))
    (inversion ((not inversion) (list 'inversion $2))
               ((comparison) $1))
    (comparison ((eq_sum) $1)
                ((lt_sum) $1)
                ((gt_sum) $1)
                ((sum) $1))
    (eq_sum ((sum equasion sum) (list 'eq_sum $1 $3)))
    (lt_sum ((sum less-than sum) (list 'lt_sum $1 $3)))
    (gt_sum ((sum grater-than sum) (list 'gt_sum $1 $3)))
    (sum ((sum plus term) (list 'sum_plus $1 $3))
         ((sum minus term) (list 'sum_minus $1 $3))
         ((term) $1))
    (term ((term mult factor) (list 'mult $1 $3))
         ((term division factor) (list 'division $1 $3))
         ((factor) $1))
    (factor ((plus power_stmt) (list 'plus $2))
            ((minus power_stmt) (list 'minus $2))
            ((power_stmt) $1))
    (power_stmt ((atom power factor) (list 'power $1 $3))
                ((primary) $1))
    (primary ((atom) $1)
             ((primary bracket-begin expression bracket-end) (list 'primary_bracket $1 $3))
             ((primary parenthes-begin parenthes-end) (list 'primary_noparam $1))
             ((primary parenthes-begin arguments parenthes-end) (list 'primary_args $1 $3)))
    (arguments ((expression) (list $1))
               ((arguments comma expression) (append $1 (list $3))))
    (atom ((ID) (list 'ID $1))
          ((True) (list 'True))
          ((False) (list 'False))
          ((None) (list 'None))
          ((NUM) (list 'NUM $1))
          ((list_stmt) $1))
    (list_stmt ((bracket-begin expressions bracket-end) (list 'list $2))
               ((bracket-begin bracket-end) (list 'empty_list)))
    (expressions ((expressions comma expression) (append $1 (list $3)))
                 ((expression) (list $1))))))
    
    


(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this python-lexer (open-input-string "a = f(1, 2, 3);")))
(let ((parser-res (python-parser my-lexer))) parser-res)


; (define-datatype expression expression?
;      (disjunction-exp
;           (var disjunction?))
; )
; (define-datatype disjunction disjunction?
;      (conjunction-exp
;           (var conjunction?))
;      (dis-or-conj-exp
;           (first disjunction?)
;           (rest conjunction?))
; )
; (define-datatype conjunction conjunction?
;      (inversion-exp
;           (var inversion?))
;      (conj-and-inv-exp
;           (first conjunction?)
;           (rest inversion?))
; )
; (define-datatype inversion inversion?
;      (not-inv-exp
;           (var inversion?))
;      (comparison-exp
;           (var comparison?))
; )


(define-datatype expression expression?
     (or-exp
          (left expression?)
          (right expression?))
     (and-exp
          (left expression?)
          (right expression?))
     (not-exp
          (var expression?))
     (equal-exp ; sum == sum
          (left expression?)
          (right expression?))
     (lt-exp ; sum < sum
          (left expression?)
          (right expression?))
     (gt-exp ; sum > sum
          (left expression?)
          (right expression?))
     (add-exp ; sum + term
          (left expression?)
          (right expression?))
     (sub-exp
          (left expression?)
          (right expression?))
     (mult-exp
          (left expression?)
          (right expression?))
     (div-exp
          (soorat expression?)
          (makhraj expression?))
     (pos-exp ; + power
          (var expression?))
     (neg-exp ; - power
          (var expression?))
     (pow-exp ; atom ** factor
          (atom expression?)
          (factor expression?))
     (bracket-exp ; primary [expression]
          (primary expression?)
          (in-bracket expression?))
     (no-arg-func-exp ; primary ()
          (primary expression?))
     (with-arg-func-exp ; primary (arguments)
          (primary expression?)
          (args list?))
     (free-bracket-exp) ; []
     (list-exp ; [expression]
          (var list?))
     (num-exp
          (num number?))
     (id-exp
          (name identifier?))
     (true-exp)
     (false-exp)
     (none-exp)
)


(define-datatype program program? 
     (prog
          (var statement?))
)

;(define-datatype statements statements? 
;     (stmt_semi
;          (var statement?))
;     (stmts_stmt_semi
;          (first statements?)
;          (rest statement?))  
;)

;(define-datatype statement statement? 
;     (Compound_stmt
;          (var  Compound_stmt?)) 
;     (Simple_stmt
;          (var Simple_stmt?)) 
;)

;(define-datatype Simple_stmt Simple_stmt? 
;     (Assignment
;          (var  Assignment?)) 
;     (Global_stmt
;          (var Global_stmt?)) 
;     (Return_stmt
;          (var Return_stmt?))
;)

;(define-datatype Compound_stmt Compound_stmt? 
;     (Function_def
;          (var  Function_def?)) 
;     (If_stmt
;          (var If_stmt?)) 
;     (For_stmt
;          (var For_stmt?))
;)

(define-datatype statement statement?
     (statements
          (stmts statement?)
          (stmt statement?))
     (break)
     (pass)
     (continue)
     (assignment
          (ID identifier?)
          (exp expression?))
     (return-value
          (value expression?))
     (return-none)
     (global_stmt
          (ID identifier?))  
     (function-with-param
          (ID identifier?)
          (params statement?)
          (body statement?))
     (function-without-param
          (ID identifier?)
          (body statement?))
     (params
          (params statement?)
          (param-with-default statement?))
     (param-with-default
          (ID identifier?)
          (exp expression?))
     (if_stmt
          (condition expression?)
          (statements statement?)
          (else_block statement?))
     (else_block
          (statements statement?))
     (For_stmt
          (ID identifier?)
          (list expression?)
          (body statement?))
)

(define-datatype python-val py-val?
  (int-val
   (val exact-integer?))
  (float-val
   (val flonum?))
  (bool-val
   (val boolean?))
  (list-val
   (val list?))
  (none-val))

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (ID identifier?)
   (val py-val?)
   (saved-env environment?))
  (extend-env-function
   (ID identifier?)
   (body statement?)
   (func-env environment?)))
