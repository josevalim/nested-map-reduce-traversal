(def input
  [{:title "Getting started",
    :reset-lesson-position? false,
    :lessons [{:name "Welcome"},
              {:name "Installation"}]},
   {:title "Basic operator",
    :reset-lesson-position? false,
    :lessons [{:name "Addition / Subtraction"},
              {:name "Multiplication / Division"}]}
   {:title "Advanced topics",
    :reset-lesson-position? true,
    :lessons [{:name "Mutability"},
              {:name "Immutability"}]}])


(defn problem-reducer
  [result {:keys [reset-lesson-position?] :as section}]
  (let [{:keys [lessons position] :or {position 0}} (last result)
        lesson-pos (if reset-lesson-position?
                     1
                     (-> lessons (last) (:position 0) (inc)))]
    (conj result
          (-> section
              (assoc :position (inc position))
              (update :lessons #(map-indexed
                                   (fn [i lesson]
                                     (assoc lesson :position (+ lesson-pos i)))
                                   %))))))

(reduce problem-reducer [] input)
;; => [{:title "Getting started", :reset-lesson-position? false, :lessons ({:name "Welcome", :position 1} {:name "Installation", :position 2}), :position 1} {:title "Basic operator", :reset-lesson-position? false, :lessons ({:name "Addition / Subtraction", :position 3} {:name "Multiplication / Division", :position 4}), :position 2} {:title "Advanced topics", :reset-lesson-position? true, :lessons ({:name "Mutability", :position 1} {:name "Immutability", :position 2}), :position 3}]
