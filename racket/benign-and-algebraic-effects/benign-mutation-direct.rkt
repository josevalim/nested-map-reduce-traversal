#lang racket

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

;; Direct mutation of counters, but functional update of input data
(pretty-write
  (let ((counter.section 0)  ;; starting at 0 simplifies functional update logic
        (counter.lesson  0))
    (for/list ((section sections))
      (when (hash-ref section 'reset-lesson-position)
        (set! counter.lesson 0))
      (set! counter.section (+ 1 counter.section))
      (hash-set* section
                 'position counter.section
                 'lessons  (for/list ((lesson (hash-ref section 'lessons)))
                             (set! counter.lesson (+ 1 counter.lesson))
                             (hash-set lesson 'position counter.lesson))))))
