#lang scheme

(define A '((1 2 3) (4 5 6)))
(define B '((7 8 9) (1 2 3) (4 5 6)))

(define dot-prod (lambda (a b) (apply + (map * a b))))

(define matrix* (lambda (A B)
  (map (lambda (row)
    (map (lambda (column)
      (dot-prod row column)) (apply map list B))) A)))

(matrix* A B)
