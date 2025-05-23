#lang eopl

;FNC ::= <numero> (<clausula> <AND>)
;clausula ::= '(<numero> <OR>)
;AND ::= '0
;    ::='AND <clausula> <AND>
;OR  ::= '0
;    ::='OR <numero><OR>

(define fnc
 (lambda (numero clausula and)
  (list 'FNC numero (list (append clausula and)))))

(define clausula
  (lambda (numero OR)
   (if (or (eqv?(car OR) 'or) (number?(car OR)))
    (clausula (append numero (list (car OR))) (cdr OR))
     (list numero)
   )
  )
)

(define OR
  (lambda (clausula numero)
    (if (eqv? numero '())
        (list clausula)
        (append clausula(list 'or (car numero)))
    )
  ) 
 )

(define AND
  (lambda (clausula1 clausula2)
    (if (eqv? clausula2 '())
        (append clausula1 '())
        (list clausula1 'and clausula2)
    )
  ) 
 )

(define fnc->var
 (lambda (FNC)
   (if (eqv? (car FNC) 'FNC)
   (cadr FNC)
   "Not FNC format"
   )
))

(define  fnc->clausulas
 (lambda (FNC)
   (if (eqv? (car FNC) 'FNC)
    (if (null? (car (caddr FNC)))
     0
      (if (list?(car (caddr FNC))) 
      (append(list (car (caddr FNC)) (fnc->clausulas (fnc (fnc->var FNC) (cdr (caddr FNC)) '()))))
      (append '() (fnc->clausulas (fnc (fnc->var FNC) (cdr (caddr FNC)) '())))
    )
    )
     "Not FNC format"
   )
))