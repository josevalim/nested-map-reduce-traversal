(ns clojure.transducers)

(def data [{:title "Getting started"
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

(defn split-each [pred]
  (comp
   (mapcat (fn [section]
             (if (pred section)
               [:restart section]
               [section])))
   (partition-by #{:restart})
   (remove #{[:restart]})))

(def number-sections&lessons
  (comp
   (map-indexed (fn [idx section]
                  (assoc section :position (inc idx))))
   (split-each :reset_lesson_position)
   (mapcat (fn [sections]
             (eduction
              (mapcat (fn [section]
                        (eduction
                         (map (fn [lesson]
                                (assoc lesson :section section)))
                         (get section :lessons))))
              (map-indexed (fn [idx lesson]
                             (assoc lesson :position (inc idx))))
              (partition-by :section)
              (map (fn [lessons-with-section]
                     (let [section (:section (first lessons-with-section))
                           lessons (mapv #(dissoc % :section) lessons-with-section)]
                       (assoc section :lessons lessons))))
              sections)))))

(def sections
  (into []
        number-sections&lessons
        data))

(println sections)


(comment
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

  (= sections clj-out) ;;=> true

  )
