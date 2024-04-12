#lang scheme

(define A '((1 2 3) (4 5 6)))
(define B '((7 8 9) (1 2 3) (4 5 6)))

(define dot-prod (lambda (a b)
  (cond
    ((null? a) 0)
    ((null? (cdr a)) (* (car a) (car b)))
    (#t (+
          (* (car a) (car b))
          (dot-prod (cdr a) (cdr b)))))))
    
(define matrix* (lambda (A B)
  (map (lambda (row)
    (map (lambda (column)
      (dot-prod row column)) (apply map list B))) A)))

(matrix* A B)
