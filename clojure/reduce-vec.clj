(def input
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

(def expected
  [{:title "Getting started"
    :reset-lesson-position false
    :lessons [{:name "Welcome" :position 1}
              {:name "Installation" :position 2}]
    :position 1}
   {:title "Basic operator"
    :reset-lesson-position false
    :lessons [{:name "Addition / Subtraction" :position 3}
              {:name "Multiplication / Division" :position 4}]
    :position 2}
   {:title "Advanced topics"
    :reset-lesson-position true
    :lessons [{:name "Mutability" :position 1}
              {:name "Immutability" :position 2}]
    :position 3}])

(defn position [coll idx] (assoc coll :position (inc idx)))

(defn update-lessons [lessons start]
  (into [] (map-indexed #(position %2 (+ start %1))) lessons))

(defn update-section [last-section start section]
  (-> section
      (position last-section)
      (update :lessons update-lessons start)))

(defn combine-seciton [acc section]
  (let [old (peek acc)
        last-section (or (some-> old :position) 0)
        last-lesson (or (some-> old :lessons peek :position) 0)
        start (if (:reset-lesson-position section) 0 last-lesson)]

    (->> section
         (update-section last-section start)
         (conj acc))))

(defn update-sections [data] (reduce combine-seciton [] data))
(assert (= expected (update-sections input)))