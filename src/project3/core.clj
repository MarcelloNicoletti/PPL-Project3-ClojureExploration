(ns project3.core)

(defn my-pas-tri
  "Gets a value from Pascal's Triangle"
  [x]
  (println "Not Implemented")
  )

(defn my-merge
  "Merges two ascending order lists in order"
  [x]
  (println "Not Implemented")
  )

(defn my-merge-sort
  "Clojure implementation of Merge Sort"
  [x]
  (println "Not Implemented")
  )

(defn my-build-bst
  "Builds a BST from a list"
  [x]
  (println "Not Implemented")
  )

(defn my-iot
  "In order traversal of a tree produced by my-build-bst"
  [x]
  (println "Not Implemented")
  )

(defn my-reverse
  "Reverses a list passed"
  [x]
  (println "Not Implemented")
  )

(defn -main
  "Run basic tests"
  [& args]
  (println "Running basic tests for custom functions:")
  (println)
  (println "Getting the 5th row, 4th spot from Pascals Triangle.")
  (println (str "Value = " (my-pas-tri 5 4) ", should be 5."))
  (println "Printing the whole 4th row.")
  (println (map #(my-pas-tri 4 %) (range 5)))
  (println)
  (println (str "Merging (1 3 5 7 9) and (0 2 4 6 8 10)"))
  (println (my-merge '(1 3 5 7 9) '(0 2 4 6 8 10)))
  (println)
  (println "Merge sort for randomized (0 1 2 3 4 5 6 7 8 9 10)")
  (let [test (shuffle (range 11))]
    (println (str "Before = " test))
    (println (my-merge-sort test))
  )
  (println)
  (println "Building a BST from (0 1 2 3 4 5 6 7)")
  (println (my-build-bst (range 8)))
  (println)
  (println "Traversing bst in order from randomized initial built BST.")
  (let [test (my-build-bst (shuffle (range 8)))]
    (println (str "Initial bst" test))
    (println (my-iot test))
    )
  (println)
  (println "Reversing (0 1 2 3 4 5 6 7 8 9 10)")'
  (println (my-reverse (range 11)))
)
