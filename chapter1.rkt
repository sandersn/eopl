#lang racket
; (define-struct node (kind start end kids))
; (define tree
;   (node "SourceFile" 0 20
;     (list (node 
;             "VariableDeclaration" 
;             0 20 
;             (list (node "VarKeyword" 0 2 '())
;                   (node "VariableDeclarationList" 4 20
;                     (list (node "Identifier" 4 4 '())
;                           (node "Equals" 6 6 '())
;                           (node "StringLiteral" 8 20 '()))))))))
; (define (node-at-location node pos) 
;   (if (empty? (node-kids node))
;     node
;     (let ((kid (findf (λ (k) (and (<= (node-start k) pos) (<= pos (node-end k)))) (node-kids node))))
;       (if kid (node-at-location kid pos) node))))
; (display (node-kind (node-at-location tree 0)))
; (newline)
; (display (node-kind (node-at-location tree 3)))
; (newline)
; (display (node-kind (node-at-location tree 4)))
; (newline)
; (display (node-kind (node-at-location tree 5)))
; (newline)
; (display (node-kind (node-at-location tree 6)))
; (newline)
; (display (node-kind (node-at-location tree 16)))
(define/match (occurs-free? tree x)
    [(`(lambda (,ps ...) ,e) x) (and (not (memq x ps)) (occurs-free? e x))]
    [(`(if ,cond ,then ,else) x) (if (memf (λ (e) (occurs-free? e x)) (list cond then else)) #t #f)]
    [((list e1 e2) x) (or (occurs-free? e1 x) (occurs-free? e2 x))]
    [(y y) #t]
    [(y x) #f])
(define/match (occurs-bound? tree x)
  [(`(lambda (,ps ...) ,e) x) 
   (or (occurs-bound? e x) 
       (match (memq x ps)
         [(cons y _) (and (eq? x y) (occurs-free? y e))]
         [_ #f]))]
  [((list e1 e2) x) (or (occurs-bound? e1 x) (occurs-bound? e2 x))]
  [(y x) #f])

(define (test f)
  (λ (expect . xs)
    (display xs)
    (let ((result (apply f xs)))
      (if (eq? result expect)
          (display " ok")
          (display " fail")))
    (newline)))
(define tof? (test occurs-free?))
(define tob? (test occurs-bound?))
(tof? #f '(lambda (x) x) 'x)
(tof? #f '(lambda (x a b) x) 'x)
(tof? #t '(lambda (y) x) 'x)
(tof? #t '(lambda (y a b) (if a x y)) 'x)
(tof? #f '(lambda (y) x) 'y)
(tof? #f '(lambda (y a b) x) 'y)
(tof? #t 'x 'x)
(tof? #f 'y 'x)
(tob? #t '(lambda (x) x) 'x)
(tob? #t '(lambda (x a b) x) 'x)
(tob? #f '(lambda (y) x) 'x)
(tob? #f '(lambda (y a b) x) 'x)
(tob? #f '(lambda (y) x) 'y)
(tob? #f '(lambda (y a b) x) 'y)
(tob? #f 'y 'y)
(tob? #f 'y 'x)
