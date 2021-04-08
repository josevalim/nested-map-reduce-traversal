(defpackage :nested-traversal.pure-gf-walk
  (:use :cl)
  (:export #:test))

(in-package :nested-traversal.pure-gf-walk)

;;;;
;;;; A purely functional approach, no mutation.
;;;;
;;;; First I define a generic object walker (could be improved) with
;;;; user-provided methods for decomposing and recomposing objects.
;;;;
;;;; Then I define data-structures for input, output (struct
;;;; inheritance), and define methods for decomposing and recomposing
;;;; sections.
;;;;
;;;; Then I define a generic walker function that works on sections
;;;; and lessons structures, to number items based on an environment
;;;; variable that is carried along during tree-walking.
;;;;

;;; A somewhat generic but naive tree walker
;;; ========================================

(defgeneric decompose (object)
  (:documentation "list all the object's child objects")
  (:method (o)
    "no child by default"
    nil)
  (:method ((list list))
    "a list already list all its children"
    list))

(defgeneric recompose (object new-children)
  (:documentation "given an object and new children, build a new object")
  (:method (object (children null))
    "for atoms without children, return object unmodified"
    object)
  (:method ((list list) children)
    "if the original object was a list, return the new children list"
    children))

(defun walk-tree (tree function &key env)
  "Call FUNCTION on TREE and its descendants while carrying an environment ENV.

   More precisely, the user-provided FUNCTION must accept at least one
   argument, the current object being visited, and an :ENV keyword
   argument (the call is made with :ALLOW-OTHER-KEYS T)

   FUNCTION must return two values, a new object and the updated
   environment. This new object is DECOMPOSE'd to produce a list of
   child objects to be processed recursively, which gives a list of
   new child objects. They are RECOMPOSE'd with that value to produce
   the final tree object."
  (flet ((walk (u e) (walk-tree u function :env e))
         (visit (u e) (funcall function u :env e :allow-other-keys t)))
    ;; first, visit object with FUNCTION and ENV
    (multiple-value-bind (tree env) (visit tree env)
      ;; then, decompose into child items and walk recursively
      (labels ((fold (env children new-children)
                 ;; fold needs to carry the environment, as well as
                 ;; the new (reversed) list of new-children when
                 ;; processing the input list of children
                 (if children
                     (destructuring-bind (c . children) children
                       (multiple-value-bind (c env) (walk c env)
                         (fold env children (cons c new-children))))
                     (let ((children (nreverse new-children)))
                       ;; recompose with tree, return also the
                       ;; updated environment
                       (values (recompose tree children) env)))))
        (fold env (decompose tree) nil)))))

;; For example:

(let ((tree '(a (b c) (d e (f g)))))
  (assert (equalp
           '("A" ("B" "C") ("D" "E" ("F" "G")))
           (walk-tree tree
                      (lambda (u &key)
                        (typecase u
                          (symbol (symbol-name u))
                          (t u))))))
  (multiple-value-bind (new-tree env)
      (walk-tree tree
                 (lambda (u &key env)
                   (values u
                           (typecase u
                             (symbol (cons u env))
                             (t env)))))
    (assert (equalp new-tree tree))
    (assert (equalp '(G F E D C B A) env))))

;;; The actual problem to solve
;;; ===========================

;; input representation

(defstruct section title reset-lesson-position lessons)

(defstruct (lesson (:constructor lesson (name)))
  name)

(defun sample-input ()
  (list (make-section :title "Getting started"
                      :reset-lesson-position nil
                      :lessons (list (lesson "Welcome")
                                     (lesson "Installation")))
        (make-section :title "Basic operator"
                      :reset-lesson-position nil
                      :lessons (list (lesson "Addition / Subtraction")
                                     (lesson "Multiplication / Division")))
        (make-section :title "Advanced topics"
                      :reset-lesson-position t
                      :lessons (list (lesson "Mutability")
                                     (lesson "Immutability")))))

;; output representation: data-structure with positions

(defstruct (num-section (:include section))
  position)

(defstruct (num-lesson
            (:include lesson)
            (:constructor num-lesson (name position)))
  position)

(defmethod decompose ((s section))
  "Children of a section: lessons"
  (section-lessons s))

(defmethod recompose ((s num-section) lessons)
  "Attach new lessons"
  (make-num-section
   :title (num-section-title s)
   :position (num-section-position s)
   :reset-lesson-position (num-section-reset-lesson-position s)
   :lessons lessons))

;; then environment for this problem is a plist of two counters,
;; :section and :lesson. there is no dedicated type for this because
;; the usage is contained in a small part of the implementation

(defun env (&key (section 1) (lesson 1))
  (list :section section :lesson lesson))

;; the visiting generic function is called add-positions.

(defgeneric add-positions (item &key env)
  (:documentation
   "Add position values to sections and lessons in tree.
    Secondary value is updated environment.")
  (:method (obj &key env)
    "By default, return arguments as-is"
    (values obj env)))

(defmethod add-positions ((lesson-item lesson) &key env)
  "lesson to num-lesson"
  (destructuring-bind (&key section lesson) env
    (values (num-lesson (lesson-name lesson-item) lesson)
            (env :section section :lesson (1+ lesson)))))

(defmethod add-positions ((section-item section) &key env)
  "section to num-section"
  (destructuring-bind (&key section lesson) env
    (with-accessors ((title   section-title)
                     (reset   section-reset-lesson-position)
                     (lessons section-lessons))
        section-item
      (values (make-num-section :title title
                                :reset-lesson-position reset
                                :lessons lessons
                                :position section)
              (env :section (1+ section)
                   :lesson (if reset 1 lesson))))))

;; For example:

(defun test ()
  (multiple-value-bind (out env) (walk-tree (sample-input) #'add-positions :env (env))
    (assert (equalp out
                    (list (make-num-section
                           :title "Getting started"
                           :reset-lesson-position NIL
                           :lessons (list (num-lesson "Welcome" 1)
                                          (num-lesson "Installation" 2))
                           :position 1)
                          (make-num-section
                           :title "Basic operator"
                           :reset-lesson-position NIL
                           :lessons (list (num-lesson "Addition / Subtraction" 3)
                                          (num-lesson "Multiplication / Division" 4))
                           :position 2)
                          (make-num-section
                           :title "Advanced topics"
                           :reset-lesson-position T
                           :lessons (list (num-lesson "Mutability" 1)
                                          (num-lesson "Immutability" 2))
                           :position 3))))
    (assert (equalp env '(:section 4 :lesson 3)))
    (print out)
    (finish-output)))
