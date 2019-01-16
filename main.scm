; for Gauche
(define true #t)
(define false #f)

; debug utils
(define (id x) x)
(define eval-orig eval)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ;; ((variable? exp) (lookup-variable-value exp env))
        ((variable? exp) (eval-orig exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((let? exp) (eval (let->combination exp) env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (eval-orig exp env))
        ;; ((lambda? exp) (make-procedure (lambda-parameters exp)
        ;;                                (lambda-body exp)
        ;;                                env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp) (eval (and->if (operands exp)) env))
        ((or? exp) (eval (or->if (operands exp)) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

(define (test-equal exp actual expected)
    (if (eq? expected actual)
        'ok
        (error exp "expect" expected ", but got" actual)))
(define (test exp env expected)
  (let ((actual (eval exp env)))
    (test-equal exp actual expected)))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type: APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
          (eval (first-exp exps) env)
          (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ;仮引数
                   (cddr exp)))) ;本体

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-arrow-clause? clause)
  (eq? (cadr clause) '=>))
(define (cond-arrow-operand clause) (car clause))
(define (cond-arrow-operator clause) (caddr clause))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (make-cond-arrow first rest)
  ; (cond ((begin (print 42) #t) => id)
  ; は42を出力するので、
  ; (if (begin (print 42) #t) (id (begin (print 42) #t)) #f)
  ; のように変換するのではなく、
  ; (let ((t (begin (print 42) #t))) (if t (id t) #f))
  ; のように変換する必要がある。
  ;; https://kmc.hatenablog.jp/entry/2019/01/11/011314 の最初の写真
  (make-let `((t ; restの内側で使われている名前と衝突するとマズい気がする。
               ,(cond-arrow-operand first)))
            (make-if
              't
              (list (cond-arrow-operator first) 't)
              rest)))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; else節はない
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last: COND->IF" clauses)))
              ((cond-arrow-clause? first)
               (make-cond-arrow
                first
                (expand-clauses rest)))
              (else
               (make-if (cond-predicate first)
                        (sequence->exp (cond-actions first))
                        (expand-clauses rest)))))))

(define (and? exp) (tagged-list? exp 'and))
(define (and->if exps)
  (if (null? exps)
      'true
      (let ((first (car exps)))
        (make-if first (and->if (cdr exps)) 'false))))

(define (or? exp) (tagged-list? exp 'or))
(define (or->if exps)
  (if (null? exps)
      'false
      (let ((first (car exps)))
        (make-if first 'true (or->if (cdr exps))))))

(define (let-list-names bindings) (map car bindings))
(define (let-list-vals bindings) (map cadr bindings))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (caddr exp))
(define (make-let bindings body)
  (append `( ( lambda ,(let-list-names bindings) ,body))
          (let-list-vals bindings)))
(define (let? exp) (tagged-list? exp 'let))
(define (let->combination exp)
  (make-let (let-bindings exp) (let-body exp)))

;; test cases
; and
(test '(and) '() true)
(test '(and true) '() true)
(test '(and false) '() false)
(test '(and true true) '() true)
(test '(and true false) '() false)
(test '(and false true) '() false)
(test '(and false false) '() false)
; or
(test '(or) '() false)
(test '(or true) '() true)
(test '(or false) '() false)
(test '(or true true) '() true)
(test '(or true false) '() true)
(test '(or false true) '() true)
(test '(or false false) '() false)
