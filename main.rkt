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

(define (divide_address_space num page_size) ; Function that returns a list consisting of the page number and page offset by splitting a logical address according to the given page_size in KB
  (list (substring num 0 (- (string-length num) (+ (log_2 page_size) 10))) (substring num (- (string-length num) (+ (log_2 page_size) 10)) (string-length num))))

(define (log_2 x) ; Function that returns the power of a given number 
  (exact-round (/ (log x) (log 2))))

(define (page args page_table  page_size) ; Function that returns the list of physical addresses of the given args with using page_table and page_size in KB
  (cond
    ((null? args) '())
    (else (cons (string-append (list-ref page_table (binary_to_decimal (car (divide_address_space (car args) page_size)))) (cadr (divide_address_space (car args) page_size))) (page (cdr args) page_table page_size)))))

(define (find_sin value num) ; Function that returns the sine of an angle given in value by using the Taylor series expansion, up to a predefined number num
  (find_sin_radian (* pi (/ value 180)) (- num 1)))

(define (find_sin_radian value num) ; Helper function that uses the radian form of the num given in the parent function
  (cond
    ((= num 0) value)
    (else (+ (find_sin_radian value (- num 1)) (* (/ (expt -1 num) (my_factorial (+ (* 2 num) 1))) (expt value (+ (* 2 num) 1)))))))

(define (my_factorial value) ; Function that returns the factorial of a value
  (my_factorial_helper value 1))

(define (my_factorial_helper value accumulator) ; Helper function to make factorial function tail recursive
  (if (<= value 0)
    accumulator
    (my_factorial_helper (- value 1) (* value accumulator))))

;(define (myhash arg table_size))

;(define (hashed_page arg table_size page_table page_size))

;(define (split_addresses args size))

;(define (map_addresses args page_table page_size))