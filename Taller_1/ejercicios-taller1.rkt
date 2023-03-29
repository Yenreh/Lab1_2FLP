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
;n:numero or b:booleano

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


;Ejercicio 9

;Propósito:
;determinar el n ́umero de inversiones de la lista L

;Entradas:
;l:lista

;Salidas:
;n:numero

(define inversions
  (lambda (l)
    (letrec
        (
         (count-inversions
          (lambda (l n)
            [cond
              [(null? l) 0]
              [(> n (car l)) (+ 1 (count-inversions (cdr l) n))]
              [else (count-inversions (cdr l) n)]
              ] 
            )
          )
         (add-inversions
          (lambda (l)
            [cond
              [(null? l) 0]
              [else (+ (add-inversions (cdr l)) (count-inversions (cdr l) (car l)))]
              ] 
            )
          )
         )
      (add-inversions l)
      )
    )
  )

;Pruebas
(inversions '(2 3 8 6 1))
(inversions '(1 2 3 4))
(inversions '(3 2 1))


;Ejercicio 11

;Propósito:
;retornar una lista donde la posici ́on n-esima corresponde al resultado de aplicar
;la funcion F sobre los elementos en la posicion n-esima en L1 y L2.

;Entradas:
;f:funcion binaria
;l1:lista
;l2:lista

;Salidas:
;n:numero

(define zip
  (lambda (f l1 l2)
    [cond
      [(null? l1) empty]
      [else (cons (f (car l1) (car l2)) (zip f (cdr l1) (cdr l2)))]
     ]
    )
  )

;Pruebas
(zip + '(1 4) '(6 2))
(zip * '(11 5 6) '(10 9 8))


;Ejercicio 13

;Propósito:
;retornar el resultado de aplicar sucesivamente las operaciones en lrators a los valores en lrands.

;Entradas:
;f:funcion binaria
;lrators:lista de operaciones binarias de tamaño n
;lrands:lista de numeros de tamaño n+1

;Salidas:
;n:numero

(define operate
  (lambda (lrators lrands)
    (letrec
        (
         (operations
          (lambda (lrators lrands aux ctr)
            (cond
              [(and (zero? ctr) (null? (cdr lrators))) ((car lrators) (car lrands) (cadr lrands))]
              [(null? (cdr lrators)) ((car lrators) aux (cadr lrands))]
              [(zero? ctr) (operations (cdr lrators) (cdr lrands) (+ aux ((car lrators) (car lrands) (cadr lrands))) (+ 1 ctr))]
              [else (operations (cdr lrators) (cdr lrands) ((car lrators) aux (cadr lrands)) (+ 1 ctr))]
              )
            )
          )
         )
      (operations lrators lrands 0 0)
      )
    )
  )

;Pruebas
(operate (list + * + - *) '(1 2 8 4 11 6))
(operate (list *) '(4 5))


;Ejercicio 15

;Propósito:
;retornar una lista con dos elementos correspondientes a la cantidad de pares e impares en arbol

;Entradas:
;arb:arbol binario

;Salidas:
;l:lista

(define count-odd-and-even
  (lambda (arb)
    (letrec
        (
         (count-filter-tree
          (lambda (arb p)
            (cond
              [(null? arb) 0]
              [(p (car arb)) (+ 1 (count-filter-tree (cadr arb) p) (count-filter-tree (caddr arb) p)) ]
              [else (+ (count-filter-tree (cadr arb) p) (count-filter-tree (caddr arb) p)) ]   
              )
            )
          )
         )
      (list (count-filter-tree arb even?) (count-filter-tree arb odd?))
      )
    )
  )

;Pruebas
(count-odd-and-even '(14
                      (7 ()
                         (12 () ()))
                      (26
                       (20
                        (17 () ())
                        ())
                       (31 () ()))))


;Ejercicio 17

;Propósito:
;retornar el resultado de realizar la multiplicacion matriz por vector

;Entradas:
;mat:lista de listas
;vec:lista

;Salidas:
;l:lista

(define prod-scalar-matriz
  (lambda (mat vec)
    (letrec
        (
         (operate-row-x-vec
          (lambda (vec mat-row)
            (if (null? vec) empty
                (cons (* (car mat-row) (car vec)) (operate-row-x-vec (cdr vec) (cdr mat-row)))
                )
            )
          )
         (join-rows
          (lambda (mat)
            (if (null? mat) empty
                (cons (operate-row-x-vec (car mat) vec) (join-rows (cdr mat)))
                )
            )
          )
         )
      (join-rows mat)
      )     
    )
  )

;Pruebas
(prod-scalar-matriz '((1 1) (2 2)) '(2 3))
(prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3))