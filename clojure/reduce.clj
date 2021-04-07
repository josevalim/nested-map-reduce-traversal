; Clojure implementation

; example data structure
(def data [
  {
    :title "Getting started"
    :reset_lesson_position false
    :lessons [
      {:name "Welcome"}
      {:name "Installation"}
    ]
  }
  {
    :title "Basic operator"
    :reset_lesson_position false
    :lessons [
      {:name "Addition / Subtraction"}
      {:name "Multiplication / Division"}
    ]
  }
  {
    :title "Advanced topics"
    :reset_lesson_position true
    :lessons [
      {:name "Mutability"}
      {:name "Immutability"}
    ]
  }
])

(defn reset-lessons-counter?[section] (section :reset_lesson_position))

(defn update-lessons[lessons c]
  (reduce (fn [acc lesson]
            (let [[counter acc-lessons] acc
                  new-counter           (inc counter)
                  new-lesson            (assoc lesson :position counter)
                  new-lessons           (cons new-lesson acc-lessons)]
              [new-counter new-lessons]))
          [c []]
          lessons))


(defn update-section[section sc lc]
  (let [lessons-counter              (if (reset-lessons-counter? section) 1 lc)
        new-section                  (assoc section :position sc)
        lessons                      (section :lessons)
        [lesson-counter new-lessons] (update-lessons lessons lessons-counter)]
    [lesson-counter (assoc new-section :lessons new-lessons)]))


(defn update-sections[sections]
  (get
    (reduce
      (fn [acc section]
        (let [[sc lc sections] acc
              [new-lesson-counter new-section] (update-section section sc lc)
              new-section-counter              (inc sc)]
          [new-section-counter new-lesson-counter (cons new-section sections)]))
        [1 1 []]
        sections)
    2))
