(def sections
  [{:title                  "Getting started",
    :reset-lesson-position? false,
    :lessons                [{:name "Welcome"}
                             {:name "Installation"}]}
   {:title                  "Basic operator",
    :reset-lesson-position? false,
    :lessons                [{:name "Addition / Subtraction"}
                             {:name "Multiplication / Division"}]}
   {:title                  "Advanced topics",
    :reset-lesson-position? true,
    :lessons                [{:name "Mutability"}
                             {:name "Immutability"}]}])

(defn update-sections []
  (loop [section-pos 1
         lesson-pos 1
         sections sections
         result []]
    (if-not (seq sections)
      result
      (let [section (-> sections first (assoc :position section-pos))
            lesson-inc (if (:reset-lesson-position? section) 1 lesson-pos)
            lessons (map-indexed
                      (fn [i lesson]
                        (assoc lesson :position (+ i lesson-inc)))
                      (:lessons section))
            section (assoc section :lessons lessons)]
        (recur (inc section-pos)
               (+ lesson-inc (count lessons))
               (next sections)
               (conj result section))))))
