;; solution using fold-left
;; tested with
;; Chez Scheme Version 9.5.5
;; Copyright 1984-2020 Cisco Systems, Inc.

(define data '(((title . "Getting Started")
                (reset-lesson-position . #f)
                (lessons . (((name . "Welcome")) ((name . "Installation")))))
               ((title . "Basic operator")
                (reset-lesson-position . #f)
                (lessons . (((name . "Addition / Subtraction"))
                            ((name . "Multiplication / Division")))))
               ((title . "Advance topics")
                (reset-lesson-position . #t)
                (lessons . (((name . "Mutability"))
                            ((name . "Immutability")))))))

(define update-position
  (lambda (data)
    (cdr (fold-left
          (lambda (acc section index)
            (let* ((lesson-length (length (cdaddr section)))
                   (cur-position (if (cdadr section) 1 (car acc)))
                   (lesson-with-position
                    (map (lambda (alist index)
                           (append alist `((position . ,(+ index cur-position)))))
                         (cdaddr section)
                         (iota lesson-length))))
              (cons (+ cur-position lesson-length)
                    (append (cdr acc)
                            (list
                             (append (list-head section 2)
                                     `((position . ,(+ 1 index))
                                       (lessons . ,lesson-with-position))))))))
          (cons 1 (list))
          data
          (iota (length data))))))

(display (update-position data)) (newline)
