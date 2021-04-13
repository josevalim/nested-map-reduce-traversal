#lang racket
(require racket/control)

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

;; Demonstrate how multiple resumption corrupts counter state
;; Each resumption will produce an extra set of sections that numbers positions somewhat chaotically.
(pretty-write
  (let loop ((result
               (reset
                 (let ((counter.section 0)  ;; starting at 0 simplifies functional update logic
                       (counter.lesson  0))
                   (for/list ((section sections))
                     (when (hash-ref section 'reset-lesson-position)
                       (set! counter.lesson 0))
                     (define uid (shift k k))  ;; Yield until we receive a UID from an external process
                     (set! counter.section (+ 1 counter.section))
                     (hash-set* section
                                'uid      uid
                                'position counter.section
                                'lessons  (for/list ((lesson (hash-ref section 'lessons)))
                                            (set! counter.lesson (+ 1 counter.lesson))
                                            (hash-set lesson 'position counter.lesson))))))))
    (if (procedure? result)
      ;; Resume twice for each interruption
      (append (loop (result (random 10000)))
              (loop (result (random 10000))))
      result)))
