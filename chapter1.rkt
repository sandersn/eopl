#lang typed/racket
;; (require datatype)
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
(define-struct Var ([name : Symbol]))
(define-struct Lambda ([parameters : (Listof Symbol)] [body : Exp]))
(define-struct If ([cond : Exp] [then : Exp] [else : Exp]))
(define-struct App ([f : Exp] [arg : Exp]))
(define-type Exp (U Var Lambda If App))
(: occurs-free? (-> Exp Symbol Boolean))
(define/match (occurs-free? tree x)
  [((Var v) x) (eq? v x)]
  [((Lambda ps e) x) (and (not (memq x ps)) (occurs-free? e x))]
  [((If cond then else) x) (if (memf (lambda ([e : Exp]) (occurs-free? e x)) (list cond then else)) #t #f)]
  [((App e1 e2) x) (or (occurs-free? e1 x) (occurs-free? e2 x))])
(: occurs-bound? (-> Exp Symbol Boolean))
(define/match (occurs-bound? tree x)
  [((Var _) x) #f]
  [((Lambda ps e) x) (or (occurs-bound? e x) (match (memq x ps)
                                              [(cons y _) (and (eq? x y) (occurs-free? e y))]
                                              [_ #f]))]
  [((If cond then else) x) (if (memf (lambda ([e : Exp]) (occurs-bound? e x)) (list cond then else)) #t #f)]
  [((App e1 e2) x) (or (occurs-bound? e1 x) (occurs-bound? e2 x))])
(: test (All (T U ...) (-> (-> U ... U T) (-> T U ... U Void))))
(define (test f)
  (λ (expect . xs)
    (display xs)
    (let ((result (apply f xs)))
      (if (eq? result expect)
          (display " ok")
          (display " fail")))
    (newline)))
(: tof? (-> Boolean Exp Symbol Void))
(define tof? (test occurs-free?))
(: tob? (-> Boolean Exp Symbol Void))
(define tob? (test occurs-bound?))
(tof? #f (Lambda '(x) (Var 'x)) 'x)
(tof? #f (Lambda '(x a b) (Var 'x)) 'x)
(tof? #t (Lambda '(y) (Var 'x)) 'x)
(tof? #t (Lambda '(y a b) (If (Var 'a) (Var 'x) (Var 'y))) 'x)
(tof? #f (Lambda '(y) (Var 'x)) 'y)
(tof? #f (Lambda '(y a b) (Var 'x)) 'y)
(tof? #t (Var 'x) 'x)
(tof? #f (Var 'y) 'x)
(tob? #t (Lambda '(x) (Var 'x)) 'x)
(tob? #t (Lambda '(x a b) (Var 'x)) 'x)
(tob? #f (Lambda '(y) (Var 'x)) 'x)
(tob? #f (Lambda '(y a b) (Var 'x)) 'x)
(tob? #f (Lambda '(y) (Var 'x)) 'y)
(tob? #f (Lambda '(y a b) (Var 'x)) 'y)
(tob? #f (Var 'y) 'y)
(tob? #f (Var 'y) 'x)
