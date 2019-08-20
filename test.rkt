#lang play

(require "base.rkt")

(print-only-errors #t)


;;;;;;;;;;;;;;;;;;;;;;     EJERICIO 1      ;;;;;;;;;;;;;;;;;;;;;;
(define pol1 (plus 3 2 (plus 4 5 (plus 5 0 (nullp)))))
(define pol2 (plus 0 10 (plus 4 5 (plus 0 3 (plus 3 2 (plus 5 0 (nullp)))))))

;;condiciones para normalice

;;cond1 
(test (cond1 pol1)
      #f)
(test (cond1 (plus 3 3(plus 4 1(nullp))))
      #t)
(test (cond1 (nullp))
      #t)
(test(cond1(plus 1 -3 (plus 10 -4(plus 2 -5(nullp)))))
     #t)
;;cond2
(test (cond2 pol2)
      #f)
(test (cond2 pol1)
      #t)
(test (cond2 (nullp))
      #t)


;;nf?
(test (nf? pol1)
      #f)
(test (nf? pol2)
      #f)
(test (nf? (nullp))
      #t)
(test (nf? (plus 4 5 (plus 3 2 (plus 5 0 (nullp)))))
      #t)

;; sumMon

(test (sumaMon 0 2 (nullp))
      (nullp))
(test (sumaMon 2 2(plus -2 2(nullp)))
      (nullp))
(test (sumaMon 4 6(plus 1 1(nullp)))
      (plus 4 6(plus 1 1(nullp))))
(test (sumaMon 6 6 (plus 4 4 (plus 2 2 (nullp))))
      (plus 6 6 (plus 4 4 (plus 2 2 (nullp)))))
(test (sumaMon 3 3 (plus 4 4 (plus 2 2 (nullp))))
      (plus 4 4 (plus 3 3 (plus 2 2 (nullp)))))
(test (sumaMon 10 2 (plus 4 4 (plus 2 2 (nullp))))
      (plus 4 4 (plus 12 2 (nullp))))
(test (sumaMon -2 2 (plus 4 4 (plus 2 2 (nullp))))
      (plus 4 4 (nullp)))

;; normalize

(test (normalize (nullp))
      (nullp))
(test (normalize (plus 1 1(nullp)))
      (plus 1 1(nullp)))
(test (normalize (plus 0 5 (nullp)))
      (nullp))
(test (normalize (plus 1 -4(plus 1 -2(plus 3 4(nullp)))))
      (plus 3 4(plus 1 -2(plus 1 -4(nullp)))))
(test (normalize (plus 4 5 (plus 8 10 (plus 0 8 (plus 7 10 (plus 2 7 (nullp)))))))
      (plus 15 10 (plus 2 7 (plus 4 5 (nullp)))))
(test (normalize pol1)
      (plus 4 5 (plus 3 2 (plus 5 0 (nullp)))))
(test (normalize pol2)
      (plus 4 5 (plus 3 2 (plus 5 0 (nullp)))))
(test (normalize (plus 0 2 (plus 5 2 (nullp))))
      (plus 5 2 (nullp)))
(test (normalize (plus 3 5(plus 2 4(plus -1 4(nullp)))))
      (plus 3 5(plus 1 4(nullp))))
(test (normalize (plus 3 5(plus 3 10(plus 2 2(plus -1 10(plus 4 -3(nullp)))))))
      (plus 2 10(plus 3 5(plus 2 2(plus 4 -3 (nullp))))))
      
;;;;;;;;;;;;;;;;;;;;;;     EJERICIO 2      ;;;;;;;;;;;;;;;;;;;;;;

;;degree
(test (degree (plus 4 4 (plus 5 5 (plus 1 1 (nullp)))))
      5)
(test (degree (plus  1 4(plus 1 3(plus 1 2(plus 1 1(nullp))))))
      4)
(test/exn (degree (nullp))
          "El polinomio nulo no tiene grado")
(test/exn (degree (plus 1 4(plus -1 4(nullp))))
          "El polinomio nulo no tiene grado")
(test (degree (plus -1 -5(plus 1 -4(nullp))))
      -4)

;; coefficient
(test (coefficient 10 (plus 2 1 (plus 5 5 (plus 1 1 (nullp)))))
      0)
(test (coefficient 5 (plus 4 5 (plus 8 10 (plus 0 8 (plus 7 10 (plus 2 7 (nullp)))))))
      4)
(test (coefficient 0 (nullp))
      0)
(test ( coefficient 1(plus 4 5 (plus 1 1(plus 20 1(plus 1 4(nullp))))))
      21)
(test (coefficient 3 (plus 1 1 (plus 3 3(plus -3 3(nullp)))))0)


;;;;;;;;;;;;;;;;;;;;;;     EJERICIO 3      ;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;     EJERICIO 4      ;;;;;;;;;;;;;;;;;;;;;;

;;(test ((evalPoly 3) (plus 2 3 (plus -6 2 (plus 2 1 (plus -1 0 (nullp)))))) 5)