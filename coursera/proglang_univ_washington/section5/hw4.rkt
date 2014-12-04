
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
        (if (> low high)
            null
            (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
    (map  (lambda (s) (string-append s suffix)) xs))


(define (list-nth-mod xs n)
    (cond 
        [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (list-ref xs (remainder n (length xs)))]
        ))

(define (stream-for-n-steps s n)
    (if (= n 0)
        null
        (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
    (letrec ([
        f (lambda (x) (cons (if (= (remainder x 5) 0) 
                    (- x)
                    x) (lambda () (f (+ x 1)))))])
        (lambda () (f 1))))

(define dan-then-dog 
    (letrec ([f (lambda (x) (cons (if (= x 1)
                                      "dan.jpg"
                                      "dog.jpg")
                                   (lambda () (f (- x)))))])
    (lambda() (f 1))))

(define (stream-add-zero s)
    (letrec ([value (s)])
        (lambda () (cons (cons 0 (car value)) (stream-add-zero (cdr value))))))
    
(define (cycle-lists xs ys)
    (letrec ([f (lambda (x) (cons (cons (list-nth-mod xs x) (list-nth-mod ys x))
                                  (lambda () (f (+ x 1)))))])
    (lambda() (f 0))))

(define (vector-assoc v vec)
    (letrec ([f (lambda (index)
            (if (>= index (vector-length vec))
                #f
                (letrec([element (vector-ref vec index)]
                        [next_index (+ index 1)] 
                        )
                    (cond [(and (pair? element) (equal? v (car element))) element]
                           [#t (f next_index)]))))])
    (f 0)))

(define cache (make-vector 0))
(define pos 0)

(define (my-assoc v xs n)
    (letrec ([element (vector-assoc v cache)])
            (if element
                (begin
                    (print "cache hit")
                    element
                )
                (letrec ([ele (assoc v xs)])
                    (begin (vector-set! cache pos ele)
                        (set! pos (remainder (+ pos 1) n))
                        (print "add to cache")
                        (print cache)
                        ele)))))

(define (cached-assoc xs n)
    (begin (if (= 0 (vector-length cache))
        (set! cache (make-vector n #f)) (print "already init cache")) 
        (lambda (v) (my-assoc v xs n))))

(define-syntax while-less
    (syntax-rules (do)
        [(while-less e1 do e2) 
          (letrec (
          [x e1]
          [f (lambda () 
                (letrec ([y e2])
                (cond [(not (number? x)) #f]
                    [(not (number? y)) #f]
                    [(>= y x) #t]
                    [#t (f)])
                ))])
                    
          (f) )]))

