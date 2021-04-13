#lang racket
(require racket/control)

;; Define algebraic effects for mutable cells, using the delimited continuation operators "shift" and "reset".
;; This would normally appear in a library.
;; For more background, see the Eff programming language: https://www.eff-lang.org/
(define-syntax with-states
  (syntax-rules ()
    ((_ ((st st.init) ...) body ...)
     (let* ((tag (make-continuation-prompt-tag))
            (st (match-lambda*
                  ('(get)     (shift-at
                                tag k (lambda (states)
                                        ((k (hash-ref states 'st))           states       ))))
                  (`(put! ,v) (shift-at
                                tag k (lambda (states)
                                        ((k (void)               ) (hash-set states 'st v)))))))
            ...)
       ((reset-at tag (let ((result (begin body ...)))
                        (lambda (states) result)))
        (make-immutable-hash `((st . ,st.init) ...)))))))

;; General-purpose auto-incrementing counter built on mutable cells
(define (counter st)
  (case-lambda
    (()          (define count.current (st 'get))
                 (st 'put! (+ count.current 1))
                 count.current)
    ((count.new) (st 'put! count.new))))

;; Convenient notation for introducing counters with encapsulated state
(define-syntax with-counters
  (syntax-rules ()
    ((_ ((count count.init) ...) body ...)
     (with-states ((count count.init) ...)
       (let ((count (counter count)) ...)
         body ...)))))

;; Solve the actual problem.
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

(pretty-write
  (with-counters ((counter.section 1)
                  (counter.lesson  1))
    (for/list ((section sections))
      (when (hash-ref section 'reset-lesson-position)
        (counter.lesson 1))
      (hash-set* section
                 'position (counter.section)
                 'lessons  (for/list ((lesson (hash-ref section 'lessons)))
                             (hash-set lesson 'position (counter.lesson)))))))
