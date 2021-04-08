(require '[clojure.zip :as zip])

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

(def z (zip/zipper
        (fn branch [in]
          (or (vector? in)
              (get in :lessons)))
        (fn children [in]
          (or (when (vector? in)
                (mapv #(assoc % :section true) in))
              (when-let [lessons (get in :lessons)]
                (mapv #(assoc % :lesson true) lessons))))
        (fn make-node [node children]
          (if (map? node)
            (assoc node :lessons children)
            children))
        data))

(def sections
  (loop [z z
         section-pos 1
         lesson-pos 1]
    (if (zip/end? z)
      (zip/root z)
      (let [node (zip/node z)]
        (cond
          (get node :section)
          (let [lesson-pos (if (get node :reset_lesson_position)
                             1
                             lesson-pos)
                z (zip/replace z
                               (-> node
                                   (assoc :position section-pos)
                                   (dissoc :section)))]
            (recur (zip/next z)
                   (inc section-pos)
                   lesson-pos))
          (get node :lesson)
          (let [z (zip/replace z
                               (-> node
                                   (assoc :position lesson-pos)
                                   (dissoc :lesson)))]
            (recur (zip/next z)
                   section-pos
                   (inc lesson-pos)))
          :else
          (recur (zip/next z)
                 section-pos
                 lesson-pos))))))

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
