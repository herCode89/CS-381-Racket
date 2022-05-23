#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     CS 381 - Programming Lab #4		;
;										;
;  < Frankie Herbert >					 	;
;  < herbertf@oregonstate.edu >	                	;
;										;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; your code here
;member? that determines if an element e is part of list lst

(define (member? e lst)
  ; complete this function definition
  (if (null? lst)
      #f
      (if (equal? e (car lst))
          #t
          (member? e (cdr lst))
          )
      )
)

;To define set? to return #t or #f

(define (set? lst) 
  ; complete this function definition
  (if (null? lst)
      #t
      (if (member? (car lst) (cdr lst))
          #f   
  (cond ((empty? lst) #f)
        ((member? (first lst) (rest lst)))
        (else (set? (rest lst)))
        )
     )
   )
)


(define (union lst1 lst2)
  (cond ((null? lst1) lst2)
        ((member? (car lst1) lst2)
         (union (cdr lst1) lst2))
        (else (union (cdr lst1)(cons (car lst1) lst2)))))

;____________________________________________________________________________________________________________________________________

; Write a function intersect that takes the set intersection of list lst1 and list lst2
; and returns a list representing the mathematical intersection of the two lists.

(define (intersect lst1 lst2)
  (cond ((null? lst1) '())
        ((member? (car lst1) lst2)
         (cons (car lst1)
               (intersect (cdr lst1) lst2)))    
        (else (intersect (cdr lst1) lst2))))

;________________________________________________________________________________________________________________________________

; Write a function difference that takes the set difference of list lst1 minus list lst2
; and returns a list representing the mathematical difference of the two lists.

(define (difference lst1 lst2)
  (cond ((null? lst1) '())
        ((member? (car lst1) lst2)
         (difference (cdr lst1) lst2))
        (else (cons (car lst1) (difference (cdr lst1) lst2)))))          



;;;;;;;;;;;;;;;;;
;  DO NOT EDIT  ;
;;;;;;;;;;;;;;;;;
; enables testing via piping in stdin
(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))
(let loop ()
  (define line (read-line (current-input-port) 'any))
  (if (eof-object? line)
    (display "")
    (begin (print (eval (read (open-input-string line)) ns)) (newline) (loop))))