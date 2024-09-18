#lang eopl
;GRAMATICA BNF
;Implementacion de la gramatica para grafos dirigidos

;; Gramática BNF para grafos dirigidos
;; graph ::= (graph vertices edges)
;; vertices ::= (vertices lista_nodos)
;; edges ::= (edges lista_aristas)
;; lista_nodos ::= (nodo1 nodo2 ... nodoN)
;; lista_aristas ::= ((nodo1 nodo2) ... (nodoM nodoN))

;; Constructores
(define graph
  (lambda (v e)
    (list 'graph v e)))

(define vertices
  (lambda (nodes)
    (list 'vertices nodes)))

(define edges
  (lambda (aristas)
    (list 'edges aristas)))

;; Predicados
(define graph?
  (lambda (l)
    (eqv? (car l) 'graph)))

(define vertices?
  (lambda (l)
    (eqv? (car l) 'vertices)))

(define edges?
  (lambda (l)
    (eqv? (car l) 'edges)))

;; Extractores
(define graph->vertices
  (lambda (g)
    (cadr g)))

(define graph->edges
  (lambda (g)
    (caddr g)))

(define vertices->nodelist
  (lambda (v)
    (cadr v)))

(define edges->edgelist
  (lambda (e)
    (cadr e)))

;; Ejemplo de creación de un grafo dirigido
(define g1
  (graph (vertices '(a b c d))
         (edges '((a b) (c d) (c b) (a c)))))

;; Ejemplo de uso: extraer los vértices y las aristas
(graph->vertices g1)  ;; => (vertices (a b c d))
(graph->edges g1)     ;; => (edges ((a b) (c d) (c b) (a c)))
