#lang eopl
; Herney Eduardo Quintero Trochez 1528556-3743


; GRAMATICA BNF

;<graph> ::= (graph-exp <vertices> <edges>)

;<vertices> ::= (vertices-exp <vertices-list>)
;<vertices-list> ::= <vertex> | <vertex> <vertices-list>
;<vertex> ::= <symbol>

;<edges> ::= (edges-exp <edges-list>)
;<edges-list> ::= <edge> | <edge> <edges-list>

;<edge> ::= (edge-exp <src> <dst>)
;<src> ::= <symbol>
;<dst> ::= <symbol>


; DEFINICION DE LOS DATATYPES

(define-datatype vertices vertices?
  (vertices-exp (vertices-list list?)))

(define-datatype edge edge?
  (edge-exp (src symbol?) (dst symbol?)))

(define-datatype edges edges?
  (edges-exp (edges-list list?)))


(define-datatype graph graph?
  (graph-exp (vertices vertices?)
             (edges edges?)))



; EJEMPLOS

(define example-graph-1
  (graph-exp
    (vertices-exp '(a b c d))
    (edges-exp (list
                (edge-exp 'a 'b)
                (edge-exp 'c 'd)
                (edge-exp 'b 'c)
                (edge-exp 'a 'd)
                (edge-exp 'c 'a)
                ))))

(define example-graph-2
  (graph-exp
    (vertices-exp '(x y z w))
    (edges-exp (list
                (edge-exp 'x 'y)
                (edge-exp 'y 'z)
                (edge-exp 'z 'w)
                (edge-exp 'w 'x)
                (edge-exp 'x 'z)
                ))))

(define example-graph-3
  (graph-exp
    (vertices-exp '(p q r s t))
    (edges-exp (list
                (edge-exp 'p 'q)
                (edge-exp 'q 'r)
                (edge-exp 'r 's)
                (edge-exp 's 't)
                (edge-exp 't 'p)
                (edge-exp 'q 's)
                ))))

; FUNCIONES PARSE

(define PARSEBNF
  (lambda (lst)
    (cond
      [(eqv? (car lst) 'graph) (graph-exp (PARSEBNF (cadr lst)) (PARSEBNF (caddr lst))) ]
      [(eqv? (car lst) 'vertices) (vertices-exp (cadr lst))]
      [(eqv? (car lst) 'edges) (edges-exp (map parse-edge (cadr lst)))]
      )))

(define parse-edge
  (lambda (lst-pair)
    (edge-exp (car lst-pair) (cadr lst-pair))))


; EJEMPLOS PARSE

(PARSEBNF '(graph (vertices (a b c d)) (edges ((a b) (c d) (c b)))))
(PARSEBNF '(graph (vertices (e f g)) (edges ((e f) (f e) (g f)))))



; FUNCIONES UNPARSE

(define unparse-edge
  (lambda (exp)
    (cases edge exp
      (edge-exp (src dst) (list src dst)))))


(define unparse-edges
  (lambda (exp)
    (cases edges exp
      (edges-exp (edges-list) (list 'edges (map unparse-edge edges-list))))))

(define unparse-vertices
  (lambda (exp)
    (cases vertices exp
      (vertices-exp (vertices-list) (list 'vertices vertices-list)))))

(define UNPARSEBNF
  (lambda (exp)
    (cases graph exp
      (graph-exp (vertices edges) (list 'graph (unparse-vertices vertices) (unparse-edges edges))))))


; EJEMPLOS UNPARSE

(UNPARSEBNF example-graph-1)
(UNPARSEBNF example-graph-2)


; FUNCION ADD-EDGE

(define add-edge
  (lambda (graph-sntx new-edge)
    (if (vertice-exist? (extract-edges (UNPARSEBNF graph-sntx)) new-edge)
        graph-sntx
        (PARSEBNF (just-add-edge (UNPARSEBNF graph-sntx) new-edge))
    )))

;FUNCIONES AUXILIARES

;Se encarga de agregar una nueva arista
(define just-add-edge
  (lambda (unparsed-graph new-edge)
    (list (car unparsed-graph) (cadr unparsed-graph) (list (caaddr unparsed-graph) (append (car (cdaddr unparsed-graph)) (list new-edge))))
    ))
 
;Extrae la lista de aristas de un grafo dirigido
(define extract-edges
  (lambda (lst)
      (cond
      [(eqv? (car lst) 'graph) (extract-edges (caddr lst))]
      [(eqv? (car lst) 'edges) (cadr lst)]
      )))

;Comprueba si un vertice existe en una lista de aristas
(define vertice-exist?
  (lambda (lst vrt)
    (cond
      [(null? lst) #f]
      [(and (eqv? (car vrt) (caar lst)) (eqv? (cadr vrt) (cadar lst))) #t]
      [else (vertice-exist? (cdr lst) vrt)])))


;EJEMPLOS DE ADD-EDGE
(add-edge example-graph-1 '(a b))
(add-edge example-graph-1 '(c b))



; FUNCION VECINOS SALIENTES

(define vecinos-salientes
  (lambda (graph-sntx node)
    (get-vecinos-salientes (extract-edges (UNPARSEBNF graph-sntx)) node)))

; Se encarga de construir la lista de vecinos salientes teniendo una lista de a aristas
(define get-vecinos-salientes
  (lambda (list-edges node)
    (cond
      [(null? list-edges) '()]
      [(eqv? (caar list-edges) node) (cons (cadar list-edges) (get-vecinos-salientes (cdr list-edges) node))]
      [else (get-vecinos-salientes (cdr list-edges) node)])))

; EJEMPLO VECINOS SALIENTES

(vecinos-salientes example-graph-1 'c)
(vecinos-salientes example-graph-1 'a)



; FUNCION VECINOS ENTRANTES

(define vecinos-entrantes
  (lambda (graph-sntx node)
    (get-vecinos-entrantes (extract-edges (UNPARSEBNF graph-sntx)) node)))

; Se encarga de construir la lista de vecinos entrantes teniendo una lista de a aristas
(define get-vecinos-entrantes
  (lambda (list-edges node)
    (cond
      [(null? list-edges) '()]
      [(eqv? (cadar list-edges) node) (cons (caar list-edges) (get-vecinos-entrantes (cdr list-edges) node))]
      [else (get-vecinos-entrantes (cdr list-edges) node)])))

;EJEMPLO VECINOS ENTRANTES
(vecinos-entrantes example-graph-1 'd)
(vecinos-entrantes example-graph-1 'a)





                   
    