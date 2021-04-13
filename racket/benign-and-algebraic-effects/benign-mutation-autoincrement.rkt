#lang racket

;; General-purpose auto-incrementing counter
(define (counter count)
  (case-lambda
    (()          (define count.current count)
                 (set! count (+ count.current 1))
                 count.current)
    ((count.new) (set! count count.new))))

(define sections
  '(#hash((title . "Getting Started")
          (reset-lesson-position . #f)
          (lessons . (#hash((name . "Welcome"))
                      #hash((name . "Installation")))))
    #hash((title . "Basic operator")
          (reset-lesson-position . #f)
          (lessons . (#hash((name . "Addition / Subtraction"))
                      #hash((name . "Multiplication / Division")))))
    #hash((title . "Advance topics")
          (reset-lesson-position . #t)
          (lessons . (#hash((name . "Mutability"))
                      #hash((name . "Immutability")))))))

;; Mutable auto-incrementing counters, but functional update of input data
(pretty-write
  (let ((counter.section (counter 1))
        (counter.lesson  (counter 1)))
    (for/list ((section sections))
      (when (hash-ref section 'reset-lesson-position)
        (counter.lesson 1))
      (hash-set* section
                 'position (counter.section)
                 'lessons  (for/list ((lesson (hash-ref section 'lessons)))
                             (hash-set lesson 'position (counter.lesson)))))))
