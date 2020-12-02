(define apply-primop apply)

(define eval
  (lambda (exp env)
    (cond ((number? exp) exp)
          ((symbol? exp) (lookup exp env))
          ((eq? (car exp) 'quote) (cadr exp))
          ((eq? (car exp) 'lambda) (list 'closure (cdr exp) env))
          ((eq? (car exp) 'cond) (eval-cond (cdr exp) env))
          (else (apply (eval (car exp) env)
                       (evlist (cdr exp) env))))))

(define apply
  (lambda (proc args)
    (cond ((primitive? proc)
           (apply-primop (cdr proc) args))
          ((eq? (car proc) 'closure)
           (eval (cadadr proc)
                 (bind (caadr proc) args (caddr proc))))
          (else 'error))))

(define primitive?
  (lambda (proc)
    (eq? (car proc) 'primitive)))

(define evlist
  (lambda (l env)
    (cond ((eq? l '()) '())
          (else (cons (eval (car l) env)
                      (evlist (cdr l) env))))))

(define eval-cond
  (lambda (clauses env)
    (cond ((eq? clauses '()) '())
          ((eq? (caar clauses) 'else)
           (eval (cadar clauses) env))
          ((false? (eval (caar clauses) env))
           (eval-cond (cdr clauses) env))
          (else
           (eval (cadar clauses) env)))))

(define bind
  (lambda (vars vals env)
    (cons (pair-up vars vals) env)))

(define pair-up
  (lambda (vars vals)
    (cond
     ((eq? vars '())
      (cond ((eq? vals '()) '())
            (else (error 'TMA))))
     ((symbol? vars)
      (cons (cons vars vals) '()))
     ((eq? vals '()) (error 'TFA))
     (else
      (cons (cons (car vars)
                  (car vals))
            (pair-up (cdr vars)
                     (cdr vals)))))))

(define lookup
  (lambda (sym env)
    (cond ((eq? env '()) (error 'UBV))
          (else
           ((lambda (vcell)
              (cond ((eq? vcell '())
                     (lookup sym (cdr env)))
                    (else (cdr vcell))))
            (assq sym (car env)))))))

(define assq
  (lambda (sym alist)
    (cond ((eq? alist '()) '())
          ((eq? sym (caar alist))
           (car alist))
          (else
           (assq sym (cdr alist))))))


(define env0 (list (list (cons '+ (cons 'primitive +))
                         (cons '- (cons 'primitive -))
                         (cons 'cons (cons 'primitive cons))
                         (cons 'cdr (cons 'primitive cdr))
                         (cons '* (cons 'primitive *))
                         (cons '> (cons 'primitive >))
                         (cons '= (cons 'primitive =))
                         (cons '< (cons 'primitive <)))))

;; (eval '(+ 1 1) env0)
;; (eval '(cond ((> 1 2) 1) ((= 1 1) 2)) env0)
(eval '(((lambda (x) (lambda (y) (+ x y))) 3) 4) env0)
