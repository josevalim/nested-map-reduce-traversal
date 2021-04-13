#lang racket
(require racket/control)

;; Define algebraic effects for mutable state, using delimited control operators.
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

;; Demonstrate that multiple resumption does NOT corrupt counter state.
;; Each resumption will produce an extra set of sections that consistently repeat the same position numbering pattern.
(pretty-write
  (let loop ((result
               (reset
                 (with-states ((counter.section 0)  ;; starting at 0 simplifies functional update logic
                               (counter.lesson  0))
                              (for/list ((section sections))
                                (define uid (shift k k))  ;; Yield until we receive a UID from an external process
                                (when (hash-ref section 'reset-lesson-position)
                                  (counter.lesson 'put! 0))
                                (counter.section 'put! (+ 1 (counter.section 'get)))
                                (hash-set* section
                                           'uid      uid
                                           'position (counter.section 'get)
                                           'lessons  (for/list ((lesson (hash-ref section 'lessons)))
                                                       (counter.lesson 'put! (+ 1 (counter.lesson 'get)))
                                                       (hash-set lesson 'position (counter.lesson 'get)))))))))
    (if (procedure? result)
      ;; Resume twice for each interruption
      (append (loop (result (random 10000)))
              (loop (result (random 10000))))
      result)))
