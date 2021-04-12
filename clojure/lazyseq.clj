"
Aim for readability rather than code size or performance.
"

(def sections
   [{:title "Getting started"
     :reset_lesson_position false
     :lessons [{:name "Welcome"}
               {:name "Installation"}]}
    {:title "Basic operator"
     :reset_lesson_position false
     :lessons [{:name "Addition / Subtraction"}
               {:name "Multiplication / Division"}]}
    {:title "Advanced topics"
     :reset_lesson_position true
     :lessons [{:name "Mutability"}
               {:name "Immutability"}]}])

(def clj-out
    [{:title "Getting started"
      :reset_lesson_position false,
      :lessons [{:name "Welcome" :position 1}
                {:name "Installation" :position 2}],
      :position 1}
     {:title "Basic operator"
      :reset_lesson_position false,
      :lessons [{:name "Addition / Subtraction" :position 3}
                {:name "Multiplication / Division" :position 4}],
      :position 2}
     {:title "Advanced topics"
      :reset_lesson_position true,
      :lessons [{:name "Mutability" :position 1}
                {:name "Immutability" :position 2}],
      :position 3}])

;; some helper functions to do the actual changing, plus example calls
(defn visit-lesson [x pos]
  (assoc x :position pos))

(visit-lesson {:name "foo"} 3) ; {:name "foo", :position 3}

(defn visit-lessons [xs start-pos]
  (map visit-lesson xs (iterate inc start-pos)))

(visit-lessons [{:name "foo"} {:name "bar"}] 4)
; ({:name "foo", :position 4} {:name "bar", :position 5})

(defn visit-section [{:keys [lessons] :as section}
                     section-pos lesson-pos]
  (-> section
      (assoc :position section-pos)
      (assoc :lessons (visit-lessons lessons lesson-pos))))

(visit-section {:lessons [{:name "foo"} {:name "bar"}]} 3 1)
; {:lessons ({:name "foo", :position 1} {:name "bar", :position 2}),
;  :position 3}

(defn positions-seq
  "Generate a lazy sequence of 3 element vectors [section section-position lesson-position]
  that can be used as arguments to visit-section."
  ([sections sp lp]
   (when-let [section (first sections)]
     ;; reset the lesson position if needed
     (let [lp (if (:reset_lesson_position section)
                 1
                 lp)]
       (cons [section sp lp]
             (lazy-seq
               ;; recurse with the rest
               (positions-seq
                 (rest sections)
                 ;; bump the section position by one
                 (inc sp)
                 ;; increase the lesson position by the lesson count
                 (+ (count (:lessons section)) lp))))))))

(doall (positions-seq sections 1 1))
; ([{:title "Getting started",
;    :reset_lesson_position false,
;    :lessons [{:name "Welcome"} {:name "Installation"}]}
;   1
;   1]
;  [{:title "Basic operator",
;    :reset_lesson_position false,
;    :lessons
;    [{:name "Addition / Subtraction"}
;     {:name "Multiplication / Division"}]}
;   2
;   3]
;  [{:title "Advanced topics",
;    :reset_lesson_position true,
;    :lessons [{:name "Mutability"} {:name "Immutability"}]}
;   3
;   1])

(defn visit-sections [sections]
  (for [[section sp lp] (positions-seq sections 1 1)]
    (visit-section section sp lp)))

(visit-sections sections)
; ({:title "Getting started",
;   :reset_lesson_position false,
;   :lessons
;   ({:name "Welcome", :position 1} {:name "Installation", :position 2}),
;   :position 1}
;  {:title "Basic operator",
;   :reset_lesson_position false,
;   :lessons
;   ({:name "Addition / Subtraction", :position 3}
;    {:name "Multiplication / Division", :position 4}),
;   :position 2}
;  {:title "Advanced topics",
;   :reset_lesson_position true,
;   :lessons
;   ({:name "Mutability", :position 1}
;    {:name "Immutability", :position 2}),
;   :position 3})

(= (visit-sections sections)
   clj-out) ; true
