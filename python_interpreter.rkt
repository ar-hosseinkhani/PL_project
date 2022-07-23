#lang racket

(require (lib "eopl.ss" "eopl"))

(require 2htdp/batch-io)

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)


;;;----------------------------------------------------------------------------------
;;; evaluate

(define evaluate 
  (lambda (path)
    (read-file path)))


;;;------------------------------------------------------------------------------------
;;; lexer and parser

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
   ("print" (token-print))
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
                            assignment semicolon print))

(define python-parser
  (parser
   (start program)
   (end EOF)
   (error void)
   (tokens a b)
   (grammar
    (program ((statements) (prog $1)))
    (statements ((statement semicolon) $1)
                ((statements statement semicolon) (statements $1 $2)))
    (statement ((compound_stmt) $1)
               ((simple_stmt) $1))
    (simple_stmt ((assignment_stmt) $1)
                 ((global_stmt) $1)
                 ((return_stmt) $1)
                 ((print-stmt) $1)
                 ((pass) (pass))
                 ((break) (break))
                 ((continue) (continue)))
    (compound_stmt ((function_def) $1)
                   ((if_stmt) $1)
                   ((for_stmt) $1))
    (assignment_stmt ((ID assignment expression) (assignment $1 $3)))
    (return_stmt ((return) (return-none))
                 ((return expression) (return-value $2)))
    (global_stmt ((global ID) (global_stmt $2)))
    (function_def ((def ID parenthes-begin params parenthes-end colon statements) (function-with-param $2 $4 $7))
                  ((def ID parenthes-begin parenthes-end colon statements) (function-without-param $2 $6)))
    (params ((param_with_default) (list $1)) ; TODO: CHECK
            ((params comma param_with_default) (append $1 (list $3)))) ; TODO: CHECK
    (param_with_default ((ID assignment expression) (param-with-default $1 $3)))
    (if_stmt ((if expression colon statements else_block) (if_stmt $2 $4 $5)))
    (else_block ((else colon statements) $3)) ; TODO: CHECK
    (for_stmt ((for ID in expression colon statements) (For_stmt $2 $4 $6)))
    (print-stmt ((print parenthes-begin expression parenthes-end) (print-stmt $3)))
    (expression ((disjunction) $1))
    (disjunction ((conjunction) $1)
                 ((disjunction or conjunction) (or-exp $1 $3))) ; TODO: check
    (conjunction ((inversion) $1)
                 ((conjunction and inversion) (and-exp $1 $3)))
    (inversion ((not inversion) (not-exp $2))
               ((comparison) $1))
    (comparison ((eq_sum) $1)
                ((lt_sum) $1)
                ((gt_sum) $1)
                ((sum) $1))
    (eq_sum ((sum equasion sum) (equal-exp $1 $3)))
    (lt_sum ((sum less-than sum) (lt-exp $1 $3)))
    (gt_sum ((sum grater-than sum) (gt-exp $1 $3)))
    (sum ((sum plus term) (add-exp $1 $3))
         ((sum minus term) (sub-exp $1 $3))
         ((term) $1))
    (term ((term mult factor) (mult-exp $1 $3))
         ((term division factor) (div-exp $1 $3))
         ((factor) $1))
    (factor ((plus power_stmt) (pos-exp $2))
            ((minus power_stmt) (neg-exp $2))
            ((power_stmt) $1))
    (power_stmt ((atom power factor) (pow-exp $1 $3))
                ((primary) $1))
    (primary ((atom) $1)
             ((primary bracket-begin expression bracket-end) (bracket-exp $1 $3))
             ((primary parenthes-begin parenthes-end) (no-arg-func-exp $1))
             ((primary parenthes-begin arguments parenthes-end) (with-arg-func-exp $1 $3)))
    (arguments ((expression) (list $1)) ; TODO: CHECK
               ((arguments comma expression) (append $1 (list $3)))) ; TODO: CHECK
    (atom ((ID) (id-exp $1))
          ((True) (true-exp)) ; TODO: CHECK
          ((False) (false-exp))
          ((None) (none-exp))
          ((NUM) (num-exp $1))
          ((list_stmt) $1))
    (list_stmt ((bracket-begin expressions bracket-end) (list-exp $2))
               ((bracket-begin bracket-end) (free-bracket-exp)))
    (expressions ((expressions comma expression) (append $1 (list $3))) ; TODO: CHECK
                 ((expression) (list $1)))))) ; TODO: CHECK

;;;----------------------------------------------------------------------------------
;;; datatype statement and expression and program

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
          (name string?))
     (true-exp)
     (false-exp)
     (none-exp)
)

(define-datatype program program? 
     (prog
          (var statement?))
)

(define-datatype statement statement?
     (statements
          (stmts statement?)
          (stmt statement?))
     (break)
     (pass)
     (continue)
     (assignment
          (ID string?)
          (exp expression?))
     (return-value
          (value expression?))
     (return-none)
     (global_stmt
          (ID string?))  
     (function-with-param
          (ID string?)
          (params list?)
          (body statement?))
     (function-without-param
          (ID string?)
          (body statement?))
     (param-with-default
          (ID string?)
          (exp expression?))
     (if_stmt
          (condition expression?)
          (statements statement?)
          (else_block statement?))
     (For_stmt
          (ID string?)
          (lst expression?)
          (body statement?))
     (print-stmt
          (var expression?))
)

(define-datatype stmt-status status?
  (pass-st)
  (break-st)
  (cont-st)
  (ret-none-st)
  (ret-val-st
   (val py-val?)))

;;;------------------------------------------------------------------------------------------
;;; values

(define-datatype python-val py-val?
  (int-val
   (val exact-integer?))
  (float-val
   (val flonum?))
  (bool-val
   (val boolean?))
  (list-val
   (val list?))
  (none-val)
  (proc-val
   (name string?)
   (body statement?)
   (env environment?)))

(define py-val->int
  (lambda (val)
    (cases python-val val
      (int-val (v) v)
      (else '()))))

(define py-val->float
  (lambda (val)
    (cases python-val val
      (float-val (v) v)
      (else '()))))

(define py-val->bool
  (lambda (val)
    (cases python-val val
      (bool-val (v) v)
      (else '()))))

(define py-val->list
  (lambda (val)
    (cases python-val val
      (list-val (v) v)
      (else '()))))

(define py-val->proc
  (lambda (val)
    (cases python-val val
      (proc-val (name body env) (list body (extend-env name val env)))
      (else '()))))

;;;-----------------------------------------------------------------------------------
;;; environment

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (ID string?)
   (val py-val?)
   (saved-env environment?)))

(define apply-env
  (lambda(env ID)
    (cases environment env
      (empty-env () '())
      (extend-env (saved-ID saved-val saved-env)
                  (if (equal? ID saved-ID) saved-val
                      (apply-env saved-env ID))))))

;;;--------------------------------------------------------------------------------------
;;; interpret statements

(define get-func-env
  (lambda (params env glob-env globs)
    (if (null? params) (list (empty-env) env)
        (cases statement (car params)
          (param-with-default (ID exp)
                              (let ([fe-e (get-func-env (cdr params) env glob-env globs)])
                                (let ([v-e (value-of exp (cadr fe-e) glob-env globs)])
                                  (list (extend-env ID (car v-e) (car fe-e)) (cadr v-e)))))
          (else '())))))
                                            
(define interpret-for
  (lambda (ID lst body env g-env globs)
    (if (null? lst) (list env g-env globs)
        (let ([e-ge-gs-st (interpret body (extend-env ID (car lst) env) g-env globs)])
          (cases stmt-status (cadddr e-ge-gs-st)
            (break-st () (list (car e-ge-gs-st) (cadr e-ge-gs-st) (caddr e-ge-gs-st) (pass-st)))
            (ret-none-st () e-ge-gs-st)
            (ret-val-st (val) e-ge-gs-st)
            (else (interpret-for ID (cdr lst) body (car e-ge-gs-st) (cadr e-ge-gs-st) (caddr e-ge-gs-st))))))))

(define interpret-prog
  (lambda (p)
    (cases program p
      (prog (stmt) (let ([x (interpret stmt (empty-env) (empty-env) '())]
                         [y (displayln "")])
                     (displayln "program finished"))))))

(define interpret
  (lambda (stmt env glob-env globs)
    (cases statement stmt
      (statements (stmts this-stmt)
                  (let ([e-ge-gs-st (interpret stmts env glob-env globs)])
                    (cases stmt-status (cadddr e-ge-gs-st)
                      (cont-st () (list (car e-ge-gs-st) (cadr e-ge-gs-st) (caddr e-ge-gs-st) (pass-st)))
                      (break-st () e-ge-gs-st)
                      (ret-none-st () e-ge-gs-st)
                      (ret-val-st (val) e-ge-gs-st)
                      (pass-st () (interpret this-stmt (car e-ge-gs-st) (cadr e-ge-gs-st) (caddr e-ge-gs-st))))))
      (assignment (ID exp)
                  (let ([v-e (value-of exp env glob-env globs)])
                    (if (= (length globs) (length (remove ID globs)))
                        (list (extend-env ID (car v-e) (cadr v-e)) glob-env globs (pass-st))
                        (list (cadr v-e) (extend-env ID (car v-e) glob-env) globs (pass-st)))))
      (global_stmt (ID) (list env glob-env (cons ID globs) (pass-st)))
      (if_stmt (condition body else-block)
               (let ([v-e (value-of condition env glob-env globs)])
                 (if (py-val->bool (car v-e))
                     (interpret body (cadr v-e) glob-env globs)
                     (interpret else-block (cadr v-e) glob-env globs))))
      (function-without-param (ID body)
                              (list (extend-env ID (proc-val ID body (empty-env)) env) glob-env globs (pass-st)))
      (function-with-param (ID params body)
                           (let ([fe-e (get-func-env (reverse params) env glob-env globs)])
                             (list (extend-env ID (proc-val ID body (car fe-e)) (cadr fe-e)) glob-env globs (pass-st))))
      (For_stmt (ID lst body)
                (let ([v-e (value-of lst env glob-env globs)])
                  (interpret-for ID (py-val->list (car v-e)) body (cadr v-e) glob-env globs)))
      (pass () (list env glob-env globs (pass-st)))
      (break () (list env glob-env globs (break-st)))
      (continue () (list env glob-env globs (cont-st)))
      (return-none () (list env glob-env globs (ret-none-st)))
      (return-value (exp)
                    (let ([v-e (value-of exp env glob-env globs)])
                     (list (cadr v-e) glob-env globs (ret-val-st (car v-e)))))
      (param-with-default (ID exp) '())
      (print-stmt (exp) (let ([v-e (value-of exp env glob-env globs)])
                          (let ([x (displayln (get-print-string (car v-e)))])
                            (list (cadr v-e) glob-env globs (pass-st))))))))
                            

(define get-list-string
  (lambda (lst)
    (cond
      [(null? lst) ""]
      [(null? (cdr lst)) (get-print-string (car lst))]
      [else (string-append (get-print-string (car lst)) (get-list-string (cdr lst)))])))
                                      
(define get-print-string
  (lambda (value)
    (cases python-val value
      (int-val (val) (number->string val))
      (float-val (val) (number->string val))
      (bool-val (val) (if val "True" "False"))
      (list-val (lst) (string-append "[" (get-list-string lst) "]"))
      (none-val "None"))))
    
                                      
;;;------------------------------------------------------------------------------------
;;; value-of expressions

(define value-of-list
  (lambda (lst env g-env globs)
    (if (null? lst) (list '() env)
        (let ([v-e (value-of (car lst) env g-env globs)])
          (let ([l-e (value-of-list (cdr lst) (cadr v-e) g-env globs)])
            (list (cons (car v-e) (car l-e)) (cadr l-e)))))))

(define fix-func-env
  (lambda (l f-env)
    (cases environment f-env
      (empty-env () (list l f-env))
      (extend-env (ID val s-env) (let ([l-fe (fix-func-env l s-env)])
                                   (if (null? (car l-fe)) (list '() (extend-env ID val (cadr l-fe)))
                                       (list (cdr (car l-fe)) (extend-env ID (car (car l-fe)) (cadr l-fe)))))))))

(define value-of
  (lambda (exp env g-env globs)
    (cases expression exp
      (true-exp () (list (bool-val #t) env))
      (false-exp () (list (bool-val #f) env))
      (none-exp () (list (none-val) env))
      (num-exp (num) (if (exact-integer? num) (list (int-val num) env)
                         (list (float-val num) env)))
      (id-exp (ID) (if (= (length (remove ID globs)) (length globs))
                       (list (apply-env env ID) env)
                       (list (apply-env g-env ID) env)))
      (list-exp (lst) (let ([l-e (value-of-list lst env g-env globs)])
                        (list (list-val (car l-e)) (cadr l-e))))
      (free-bracket-exp () (list (list-val '()) env))
      (with-arg-func-exp (pri args) (let ([v-e (value-of pri env g-env globs)])
                                      (let ([l-e (value-of-list args (cadr v-e) g-env globs)])
                                        (let ([b-e (py-val->proc (car v-e))])
                                          (let ([l-fe (fix-func-env (car l-e) (cadr b-e))])
                                            (let ([fe-e-gs-rt (interpret (car b-e) (cadr l-fe) (cadr l-e) '())])
                                              (cases stmt-status (cadddr fe-e-gs-rt)
                                                (ret-val-st (val) (list val (cadr fe-e-gs-rt)))
                                                (else (list (none-val) (cadr fe-e-gs-rt))))))))))
      (no-arg-func-exp (pri) (let ([v-e (value-of pri env g-env globs)])
                               (let ([b-e (py-val->proc (car v-e))])
                                 (let ([fe-e-gs-rt (interpret (car b-e) (cadr b-e) (cadr v-e) '())])
                                   (cases stmt-status (cadddr fe-e-gs-rt)
                                     (ret-val-st (val) (list val (cadr fe-e-gs-rt)))
                                     (else (list (none-val) (cadr fe-e-gs-rt))))))))
      (bracket-exp (pri inb) (let ([p-e (value-of pri env g-env globs)])
                               (let ([i-e (value-of inb (cadr p-e) g-env globs)])
                                 (list (list-ref (py-val->list (car p-e)) (py-val->int (car i-e))) (cadr i-e)))))
      (pow-exp (atom factor) (let ([a-e (value-of atom env g-env globs)])
                               (let ([f-e (value-of factor (cadr a-e) g-env globs)])
                                 (cases python-val (car a-e)
                                   (int-val (val) (list (int-val (expt val (py-val->int (car f-e)))) (cadr f-e)))
                                   (float-val (val) (list (float-val (expt val (py-val->float (car f-e)))) (cadr f-e)))
                                   (else '())))))
      (neg-exp (var) (let ([v-e (value-of var env g-env globs)])
                       (cases python-val (car v-e)
                         (int-val (val) (list (int-val (- val)) (cadr v-e)))
                         (float-val (val) (list (float-val (- val)) (cadr v-e)))
                         (else '()))))
      (pos-exp (var) (value-of var env g-env globs))
      (div-exp (top bot) (let ([t-e (value-of top env g-env globs)])
                               (let ([b-e (value-of bot (cadr t-e) g-env globs)])
                                 (cases python-val (car t-e)
                                   (int-val (val) (list (int-val (quotient val (py-val->int (car b-e)))) (cadr b-e)))
                                   (float-val (val) (list (float-val (/ val (py-val->float (car b-e)))) (cadr b-e)))
                                   (else '())))))
      (mult-exp (left right) (let ([l-e (value-of left env g-env globs)])
                               (let ([r-e (value-of right (cadr l-e) g-env globs)])
                                 (cases python-val (car l-e)
                                   (int-val (val) (list (int-val (* val (py-val->int (car r-e)))) (cadr r-e)))
                                   (float-val (val) (list (float-val (* val (py-val->float (car r-e)))) (cadr r-e)))
                                   (else '())))))
      (sub-exp (left right) (let ([l-e (value-of left env g-env globs)])
                               (let ([r-e (value-of right (cadr l-e) g-env globs)])
                                 (cases python-val (car l-e)
                                   (int-val (val) (list (int-val (- val (py-val->int (car r-e)))) (cadr r-e)))
                                   (float-val (val) (list (float-val (- val (py-val->float (car r-e)))) (cadr r-e)))
                                   (else '())))))
      (add-exp (left right) (let ([l-e (value-of left env g-env globs)])
                               (let ([r-e (value-of right (cadr l-e) g-env globs)])
                                 (cases python-val (car l-e)
                                   (int-val (val) (list (int-val (+ val (py-val->int (car r-e)))) (cadr r-e)))
                                   (float-val (val) (list (float-val (+ val (py-val->float (car r-e)))) (cadr r-e)))
                                   (else '())))))
      (gt-exp (left right) (let ([l-e (value-of left env g-env globs)])
                               (let ([r-e (value-of right (cadr l-e) g-env globs)])
                                 (cases python-val (car l-e)
                                   (int-val (val) (list (bool-val (> val (py-val->int (car r-e)))) (cadr r-e)))
                                   (float-val (val) (list (bool-val (> val (py-val->float (car r-e)))) (cadr r-e)))
                                   (else '())))))
      (lt-exp (left right) (let ([l-e (value-of left env g-env globs)])
                               (let ([r-e (value-of right (cadr l-e) g-env globs)])
                                 (cases python-val (car l-e)
                                   (int-val (val) (list (bool-val (< val (py-val->int (car r-e)))) (cadr r-e)))
                                   (float-val (val) (list (bool-val (< val (py-val->float (car r-e)))) (cadr r-e)))
                                   (else '())))))
      (equal-exp (left right) (let ([l-e (value-of left env g-env globs)])
                               (let ([r-e (value-of right (cadr l-e) g-env globs)])
                                 (list (bool-val (equal? (car l-e) (car r-e))) (cadr r-e)))))
      (not-exp (var) (let ([v-e (value-of var env g-env globs)])
                       (list (bool-val (not (py-val->bool (car v-e)))) (cadr v-e))))
      (and-exp (left right) (let ([l-e (value-of left env g-env globs)])
                               (let ([r-e (value-of right (cadr l-e) g-env globs)])
                                 (list (bool-val (and (py-val->bool (car l-e)) (py-val->bool (car r-e)))) (cadr r-e)))))
      (or-exp (left right) (let ([l-e (value-of left env g-env globs)])
                               (let ([r-e (value-of right (cadr l-e) g-env globs)])
                                 (list (bool-val (or (py-val->bool (car l-e)) (py-val->bool (car r-e)))) (cadr r-e))))))))
;;;------------------------------------------------------------------------------------------
;;; run code
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this python-lexer (open-input-string (evaluate "in7.txt"))))
(interpret-prog (let ((parser-res (python-parser my-lexer))) parser-res))
