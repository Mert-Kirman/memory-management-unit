;name surname
;student id
;compiling: yes
;complete: yes
#lang racket

(provide (all-defined-out))


(define (binary_to_decimal binary) ; Function that converts a binary string to a decimal number
  (if (= (string-length binary) 1)
      (string->number binary)
      (+ (string->number (string (string-ref binary (- (string-length binary) 1)))) (* 2 (binary_to_decimal (substring binary 0 (- (string-length binary) 1)))))))

(define (relocator args limit base) ; Function that returns the corresponding physical addresses for a list of logical addresses in binary format, given in the list args
  (cond
    ((null? args) '())
    (else (cons (if (> (binary_to_decimal (car args)) limit)
                    -1
                    (+ (binary_to_decimal (car args)) base))
                (relocator (cdr args) limit base)))))

(define (divide_address_space num page_size)
  (list (substring num 0 (- (string-length num) (+ (log_2 page_size) 10))) (substring num (- (string-length num) (+ (log_2 page_size) 10)) (string-length num))))

(define (log_2 x) ; Function that returns the power of a given number 
  (exact-round (/ (log x) (log 2))))

;(define (page args page_table  page_size))

;(define (find_sin value num))

;(define (myhash arg table_size))

;(define (hashed_page arg table_size page_table page_size))

;(define (split_addresses args size))

;(define (map_addresses args page_table page_size))