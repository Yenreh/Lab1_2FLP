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


; DEFINICION CON LISTAS


; Constructor para el grafo
(define make-graph
  (lambda (vertices edges)
    (list 'graph vertices edges)))

; Constructor para la lista de vertices
(define make-vertices
  (lambda (nodelist)
    (list 'vertices nodelist)))

; Constructor para la lista de aristas
(define make-edges
  (lambda (edges-list)
    (list 'edges edges-list)))

; Constructor para una arista
(define make-edge
  (lambda (src dst)
    (list src dst)))

;EJEMPLOS DE LOS CONSTRUCTORES

(define example-graph-1
  (make-graph
    (make-vertices '(a b c d))
    (make-edges (list
                  (make-edge 'a 'b)
                  (make-edge 'c 'd)
                  (make-edge 'b 'c)
                  (make-edge 'a 'd)
                  (make-edge 'c 'a)))))

(define example-graph-2
  (make-graph
    (make-vertices '(x y z w))
    (make-edges (list
                  (make-edge 'x 'y)
                  (make-edge 'y 'z)
                  (make-edge 'z 'w)
                  (make-edge 'w 'x)
                  (make-edge 'x 'z)))))


(define example-graph-3
  (make-graph
    (make-vertices '(p q r s t))
    (make-edges (list
                  (make-edge 'p 'q)
                  (make-edge 'q 'r)
                  (make-edge 'r 's)
                  (make-edge 's 't)
                  (make-edge 't 'p)
                  (make-edge 'q 's)))))


; Extractor para los vertices
(define graph->vertices
  (lambda (graph)
    (cadr graph)))

; Extractor para las aristas
(define graph->edges
  (lambda (graph)
    (caddr graph)))

; Extractor para la lista de nodos
(define vertices->nodelist
  (lambda (vertices)
    (cadr vertices)))



;EJEMPLOS DE LOS EXTRACTORES

(graph->vertices example-graph-1)
(graph->vertices example-graph-2)


(graph->edges example-graph-1)
(graph->edges example-graph-2)

(vertices->nodelist (graph->vertices example-graph-1))
(vertices->nodelist (graph->vertices example-graph-2))
