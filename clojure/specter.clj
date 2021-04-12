; you will need https://github.com/redplanetlabs/specter as an dependency
; this should also work in clojurescript
(require '[com.rpl.specter :as s])

(def data [{:title                 "Getting started",
            :reset_lesson_position false,
            :lessons               [{:name "Welcome"} {:name "Installation"}]}
           {:title                 "Basic operator",
            :reset_lesson_position false,
            :lessons               [{:name "Addition / Subtraction"} {:name "Multiplication / Division"}]}
           {:title                 "Advanced topics",
            :reset_lesson_position true,
            :lessons               [{:name "Mutability"} {:name "Immutability"}]}])

;; unfortunately there is no partition standard partition function for this kind of splitting, so we need to create an own one
(defn partition-when
  "Partitions a collection whenever pred is true"
  [pred coll]
  (loop [result []
         current-col []
         [current & rest] coll]
    (cond (nil? current) (conj result current-col)
          (pred current) (recur (conj result current-col) [current] rest)
          :else (recur result (conj current-col current) rest))))

;; Specter is a lens like library to navigate into nested datastructures and transform them
;; It's pretty easy to build own navigators that are composable with all of specter

;; Navigates to all subpartitions using partition-when
(defn partitions-nav [pred]
  (s/path (s/parser #(partition-when pred %) #(reduce into %))
        s/ALL))

;; Navigates to all elements in a collection (like ALL) but collects the index
(def ALL-WITH-INDEX
  (s/path s/INDEXED-VALS (s/collect-one s/FIRST) s/LAST))

(defn run [data]
  (s/transform [(s/multi-path
                  s/STAY
                  [(partitions-nav :reset_lesson_position) (s/subselect s/ALL :lessons s/ALL)])
                ALL-WITH-INDEX :position]
             (fn [i _] (inc i)) data))

(comment
  (= (run data)
     [{:title                 "Getting started",
       :reset_lesson_position false,
       :lessons               [{:name "Welcome", :position 1} {:name "Installation", :position 2}],
       :position              1}
      {:title                 "Basic operator",
       :reset_lesson_position false,
       :lessons               [{:name "Addition / Subtraction", :position 3} {:name "Multiplication / Division", :position 4}],
       :position              2}
      {:title                 "Advanced topics",
       :reset_lesson_position true,
       :lessons               [{:name "Mutability", :position 1} {:name "Immutability", :position 2}],
       :position              3}]))