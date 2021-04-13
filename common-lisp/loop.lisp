(let ((sample (list
               (list
                'title "Getting started"
                'reset_lesson_position nil
                'lessons (list
                          (list 'name "Welcome")
                          (list 'name "Installation")
                          )
                )

               (list
                'title "Basic operator"
                'reset_lesson_position nil
                'lessons (list
                          (list 'name "Addition / Subtraction")
                          (list 'name "Multiplication / Division")
                          )
                )

               (list
                'title "Advanced topics"
                'reset_lesson_position t
                'lessons (list
                          (list 'name "Mutability")
                          (list 'name "Immutability")
                          )
                )
               )))

  (loop for section in sample
        for curLevel = 1 then (if (getf section 'reset_lesson_position) 1 curLevel)
        do (loop for lesson in (getf section 'lessons)
                 do (progn (nconc lesson (list 'position curLevel))
                          (setq curLevel (1+ curLevel)))))

  sample)
