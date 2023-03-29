#lang eopl

;Nombre 1: Herney Eduardo Quintero Trochez 
;Código 1: 1528556-3743

;Nombre 2: 
;Código 2: 

;Ejercicio 1

;Propósito:
;retornar una lista similar a L, con pares ordenados invertidos, es decir, y, x,
;solo cuando ambos elementos de la lista cumplan con un predicado

;Entradas:
;l:lista
;p:predicado

;Salidas:
;l:lista

(define invert
  (lambda (l p)
    [cond
      [(null? l) empty]
      [(and (p (caar l)) (p (cadar l))) (cons (list (cadar l) (caar l)) (invert (cdr l) p))]
      [else (invert (cdr l) p)]
      ]
    )
  )

;Pruebas
(invert '((3 2) (4 2) (1 5) (2 8)) even?)
(invert '((6 9) (10 90) (82 7) ) odd? )


;Ejercicio 3

;Propósito:
;retornar una lista similar a la que recibe (L), pero debe tener en la posición ingresada n (indexando desde cero)
;el elemento x solo si el elemento original de la lista cumple con el predicado

;Entradas:
;l:lista
;n:numero
;x:elemento
;p:predicado

;Salidas:
;l:lista

(define list-set
  (lambda (l n x p)
    (letrec
        (
         (count-list-set
          (lambda (l n x p ctr)
            (cond
              [(null? l) empty]
              [(and (= ctr n) (p (car l))) (cons x (cdr l))]
              [else (cons (car l) (count-list-set (cdr l) n x p (+ ctr 1)))]
              )
            )
          )
         )
      (count-list-set l n x p 0)
      )
    )
  )

;Pruebas
(list-set '(5 8 7 6) 2 '(1 2) odd?)
(list-set '(5 8 7 6) 2 '(1 2) even?)


;Ejercicio 5

;Propósito:
;retornar (desde una posicion inicial 0) el primer elemento de la lista que satisface el predicado L.
;Si llega a suceder que ningun elemento satisface el predicado recibido, la funcion debe retornar #f


;Entradas:
;l:lista
;p:predicado

;Salidas:
;n:number or b:bool

(define list-index
  (lambda (p l)
    (letrec
        (
         (count-list-index
          (lambda (p l ctr)
            (cond
              [(null? l) #f]
              [(p (car l)) ctr]
              [else (count-list-index p (cdr l) (+ ctr 1))]
              )
            )
          )
         )
      (count-list-index p l 0)
      )
    )
  )

;Pruebas
(list-index number? '(a 2 (1 3) b 7))
(list-index symbol? '(a (b c) 17 foo))
(list-index symbol? '(1 2 (a b) 3))

;Ejercicio 7

;Propósito:
;retornar una lista de tuplas que representen el producto cartesiano entre L1y L2.
;Los pares pueden aparecer en cualquier orden


;Entradas:
;l1:lista
;l2:lista

;Salidas:
;l:lista
(define cartesian-product
  (lambda (l1 l2)
    (letrec
        (
         (join-pairs
           (lambda (e l)
             [cond
               [(null? l) empty]
               [else (cons (list e (car l)) (join-pairs e (cdr l)))]
               ]
             )
           )
         (join-lists
           (lambda (l1 l2)
             [cond
               [(null? l1) empty]
               [else (append (join-pairs (car l1) l2) (cartesian-product (cdr l1) l2))]
               ]
             )
           )
         )
      (join-lists l1 l2)
      )
    )
  )


;Pruebas
(cartesian-product '(a b c) '(x y))
(cartesian-product '(p q r) '(5 6 7))
