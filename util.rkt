#lang racket

(define (zip xs ys)
  (match (cons xs ys)
    [(cons (cons x xs_) (cons y ys_)) (cons (cons x y) (zip xs_ ys_))]
    [_ '()]))