(def data
  [{:title "Getting started"
    :reset-lesson-position false
    :lessons [{:name "Welcome"}
              {:name "Installation"}]}
   {:title "Basic operator"
    :reset-lesson-position false
    :lessons [{:name "Addition / Subtraction"}
              {:name "Multiplication / Division"}]}
   {:title "Advanced topics"
    :reset-lesson-position true
    :lessons [{:name "Mutability"}
              {:name "Immutability"}]}])

(defn update-sections
  [data]
  (let [indices (for [[section-ix section] (map vector (range) data)
                       lesson-ix (-> section :lessons count range)]
                   [section-ix lesson-ix])
        resets (for [[section-ix lesson-ix] indices]
                 (and (zero? lesson-ix) (get-in data [section-ix :reset-lesson-position])))
        lesson-positions (reductions #(if %2 1 (inc %1)) 1 (rest resets))
        data-with-sections (into [] (map-indexed #(assoc %2 :position (inc %1)) data))]
    (reduce (fn [data [section-ix lesson-ix lesson-position]]
              (assoc-in data [section-ix :lessons lesson-ix :position] lesson-position))
            data-with-sections
            (map conj indices lesson-positions))))
