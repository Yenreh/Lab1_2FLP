#lang eopl

;Nombre 1: Herney Eduardo Quintero Trochez 
;Código 1: 1528556-3743

;Nombre 2: 
;Código 2: 


;Ejercicio 1

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

