#lang eopl

;Nombre 1: Herney Eduardo Quintero Trochez 
;Código 1: 1528556-3743

;Nombre 2: 
;Código 2: 


;Ejercicio 1

;Entradas:
;-l:lista
;-p:predicado

;Salida:
;-lista

(define invert
  (lambda (l p)
    [cond
      [(null? l) empty]
      [(and (p (caar l)) (p (cadar l))) (cons (car l) (invert (cdr l) p))]
      [else (invert (cdr l) p)]
      ]
    )
  )

;Pruebas
(invert '((3 2) (4 2) (1 5) (2 8)) even?)
(invert '((6 9) (10 90) (82 7) ) odd? )


;Ejercicio 3
(define list-set
  (lambda (l n x p)
    
    )
  )
;Pruebas
;(list-set ’(5 8 7 6) 2 ’(1 2) odd?)
;(list-set ’(5 8 7 6) 2 ’(1 2) even?)
;(list-set ’(5 8 7 6) 3 ’(1 5 10) mayor5? )
;:(list-set ’(5 8 7 6) 0 ’(1 5 10) mayor5? )

