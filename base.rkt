#lang play


#|
Complete los datos personales de los miembros del grupo:
NOMBRE Y APELLIDO: Alejandra Alarcón
RUT: 19.366.375-3

NOMBRE Y APELLIDO: Sebastián Donoso
RUT: 18.880.887-5
|#


#|
<Polynomial> ::= (nullp)
              |  (plus <Number> <Integer> >Polynomial>)
|#
(deftype Polynomial
  (nullp)
  (plus coef exp rem))

;;;;;;;;;;;;;;;;;;;;;;     EJERICIO 1      ;;;;;;;;;;;;;;;;;;;;;;

;; cond1 :: Polynomial -> Bool
;; Funcion auxiliar que verifica que el polinomio tenga los exponentes ordenados
;; de mayor a menor
(define (cond1 p)
  (match p
    [(nullp) #t]
    [(plus coef1 exp1
           (plus coef2 exp2 rem))
     (cond
       [(< exp1 exp2) #f]
       [(= exp1 exp2) #f]
       [else
        (cond1 (plus coef2 exp2 rem))])]
    [(plus coef exp (nullp)) #t]))

;; cond2 :: Polynomial -> Bool
;; Funcion auxiliar que verifica que el polinomio no tenga coeficientes nulos
(define (cond2 p)
  (match p
    [(nullp) #t]
    [(plus coef exp rem)
     (if (zero? coef)
         #f
         (cond2 rem))]))

;; nf? :: Polynomial -> Bool
;; Verifica que un polinomio este en forma normal
(define (nf? p)
  (if (and (cond1 p) (cond2 p))
          #t
          #f))

;; removeZeros :: Polynomial -> Polynomial
;; quita los terminos de un polinomio con coeficiente nulo
(define (removeZeros p)
  (match p
    [(nullp) (nullp)]
    [(plus coef exp rem)
     (if (zero? coef)
         (removeZeros rem)
         (plus coef exp (removeZeros rem)))]))

;; sumaMonAux :: Number Integer Polynomial -> Polynomial
;; retorna un polinomio al sumar c*x^m con un polinomio p
;; el resultado no esta normalizado por la condicion (ii)
(define (sumaMonAux c m p)
  (match p
    [(nullp)(plus c m (nullp))]
    [(plus coef exp rem)
     (cond
       [(= m exp) (plus (+ c coef) exp rem)]
       [(< m exp) (plus coef exp (sumaMon c m rem))]
       [else (plus c m (plus coef exp rem))])]))

;; sumaMon :: Number Integer Polynomial -> Polynomial
;; retorna un polinomio en forma normal al sumar c*x^m
;; con un polinomio normalizado p
(define (sumaMon c m p)
  (removeZeros (sumaMonAux c m p)))

;; normalize :: Polynomial -> Polynomial
;; Normaliza un polinomio
(define (normalize p)
  (match p
    [(nullp) (nullp)]
    [(plus coef exp rem) (sumaMon coef exp (normalize rem))]))

;;;;;;;;;;;;;;;;;;;;;;     EJERICIO 2      ;;;;;;;;;;;;;;;;;;;;;;

;; degree :: Polynomial -> Integer
;; Devuelve el grado de un polinomio
(define (degree p)
  (match (normalize p)
    [(nullp) (error "El polinomio nulo no tiene grado")]
    [(plus coef exp rem) exp]))

;; coefficient :: Integer Polynomial -> Number
;; Devueve el coeficiente asociado a un exponente dado
(define (coefficient n p)
  (match (normalize p)
    [(nullp) 0]
    [(plus coef exp rem)
     (if (= exp n)
         coef
         (coefficient n rem))]))

;;;;;;;;;;;;;;;;;;;;;;     EJERICIO 3      ;;;;;;;;;;;;;;;;;;;;;;

;; sumaPoly :: Polynomial Polynomial -> Polynomial
;; Suma dos polinomios (no necesariamente normalizados)
(define (sumaPoly p1 p2)
  (match (normalize p1)
    [(nullp) (normalize p2)]
    [(plus coef exp rem)(normalize (sumaMon coef exp (sumaPoly rem p2)))]))

;;mapPoly :: (Number Integer -> Number * Integer) Polynomial -> Polynomial
;; devuelve el polinomio que resulta de aplicar f a cada coeficiente
;; y exponente de p
(define (mapPoly f p)
  (match p
    [(nullp) (nullp)]
    [(plus coef exp rem) (plus (car (f coef exp)) (cdr (f coef exp)) (mapPoly f rem))]))

;; multPoly :: Polynomial Polynomial -> Polynomial
;; Multiplica dos polinomios (no necesariamente normalizados)
(define (multPoly p1 p2)
  (match p1
    [(nullp) (nullp)]
    [(plus coef exp rem)
     (sumaPoly (mapPoly (λ(x y) (cons (* x coef)(+ y exp))) p2) (multPoly rem p2))]))

;;;;;;;;;;;;;;;;;;;;;;     EJERICIO 4      ;;;;;;;;;;;;;;;;;;;;;;
;; foldPoly :: A (Number Integer A -> A) -> (Polynomial -> A)
(define (foldPoly a f)
  (λ(p)
    (match p
      [(nullp) a]
      [(plus c g r) (f c g ((foldPoly a f) r))])))

;; evalPoly :: Number -> (Polynomial -> Number)
;; Evalua un polinomio en un valor dado
(define (evalPoly v)
  (foldPoly 0 (λ(x y z) (+ (* x (expt v y)) z))))