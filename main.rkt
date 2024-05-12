;name surname
;student id
;compiling: yes
;complete: yes
#lang racket

(provide (all-defined-out))


(define (binary_to_decimal binary)
  (if (= (string-length binary) 1)
      (string->number binary)
      (+ (string->number (string (string-ref binary (- (string-length binary) 1)))) (* 2 (binary_to_decimal (substring binary 0 (- (string-length binary) 1)))))))

(define (relocator args limit base)
  (cond
    ((null? args) '())
    (else (cons (if (> (binary_to_decimal (car args)) limit)
                    -1
                    (+ (binary_to_decimal (car args)) base))
                (relocator (cdr args) limit base)))))

;(define (divide_address_space num page_size))

;(define (page args page_table  page_size))

;(define (find_sin value num))

;(define (myhash arg table_size))

;(define (hashed_page arg table_size page_table page_size))

;(define (split_addresses args size))

;(define (map_addresses args page_table page_size))