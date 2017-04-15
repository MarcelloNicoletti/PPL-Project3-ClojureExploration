(ns project3.core)

;; note that the function "+'" does the same as "+" but
;; it automaticaly promotes numbers to BigInt as needed.

(defn add-pairs
  "Adds the pairs of numbers together."
  [x]
  (if (= 1 (count x))
    x
    (cons (+' (first x) (second x)) (add-pairs (rest x)))
  )
)

(defn add-pairs'
  "Adds the pairs of numbers together from the back."
  [x]
  (reverse (add-pairs (reverse x)))
)

(defn my-pas-tri-row
  "Gets a list representing a row in Pascal's Triangle"
  [n]
  (cond
    (< n 0) nil
    (= n 0) '(1)
    :else
      (add-pairs (cons 0 (my-pas-tri-row (- n 1))))
  )
)

(defn pad
  "Returns a collection n long from coll, padded with val"
  [n val coll]
  ;; repeat and concat are lazy sequences so stop once take is satisfied
  (take n (concat coll (repeat val)))
)

(defn my-pas-tri-row'
  "Gets a list representing a row in Pascal's Triangle"
  ([n k]
    (if (>= 0 n)
      '(1)
      (add-pairs' (pad (+ (min k n) 1) 0 (my-pas-tri-row' (- n 1) k)))
    )
  )
  ([n]
    (my-pas-tri-row' n (+ n 1))
  )
)

(defn my-pas-tri
  "Gets a value from Pascal's Triangle by first calculating the row."
  [n k]
  (if (< n k)
    nil
    (nth (my-pas-tri-row n) k)
    )
  )

(defn my-pas-tri'
  "Gets a value from Pascal's Triangle by first calculating the row."
  [n k]
  (cond
    (< n k) nil
    (> k (/ n 2)) (nth (my-pas-tri-row' n (- n k)) (- n k))
    :else (nth (my-pas-tri-row' n k) k)
  )
)

(defn my-merge
  "Merges two ascending order lists in order."
  [x y]
  (if (empty? x)
    ;; x is empty return y (which still may be empty)
    y
    (if (empty? y)
      ;; y is empty return x
      x
      ;; They're both populated, merge them
      (if (< (first x) (first y))
        (cons (first x) (my-merge (rest x) y))
        (cons (first y) (my-merge x (rest y)))
      )
    )
  )
)

(defn my-merge-sort
  "Clojure implementation of Merge Sort."
  [x]
  (if (= 1 (count x))
    x
    (let [[left right] (split-at (/ (count x) 2) x)]
      (my-merge
        (my-merge-sort left)
        (my-merge-sort right)
      )
    )
  )
)

(defn bst-new-node
  "Makes a new node list form."
  [x]
  (list x nil nil)
)

(defn my-bst-insert
  "Helper function to my-build-bst. performs recursive insert"
  [x node]
  (if (= 3 (count node))
    (let [key (nth node 0) left (nth node 1) right (nth node 2)]
      (cond
        (< x key)
          (if (empty? left)
            (list key (bst-new-node x) right)
            (list key (my-bst-insert x left) right)
          )
        (> x key)
        (if (empty? right)
          (list key left (bst-new-node x))
          (list key left (my-bst-insert x right))
        )
        :else
          node
      )
    )
  )
)

(defn my-build-bst
  "Builds a BST from a list"
  [x]
  (if (= 1 (count x))
    (bst-new-node (last x))
    (my-bst-insert (last x) (my-build-bst (butlast x)))
  )
)

(defn my-bst-in-order
  "In order traversal of a tree produced by my-build-bst"
  [node]
  (if (= 3 (count node))
    (let [key (nth node 0) left (nth node 1) right (nth node 2)]
      (concat (my-bst-in-order left) (cons key (my-bst-in-order right)))
    )
    ()
  )
)

(defn my-reverse-aux
  "Reverses a list passed"
  [x]
  (if (empty? x)
    [] ;; Return an empty vector. Vector's conj side is the back
    (conj (my-reverse-aux (rest x)) (first x))
  )
)

(defn my-reverse
  "Reverses a list passed"
  [x]
  (seq (my-reverse-aux x))
)

(defn my-fib
  "Recursive Fib sequence"
  [x]
  (if (<= x 1)
    1
    (+ (my-fib (- x 1)) (my-fib (- x 2)))
  )
)

(defn -main
  "Run basic tests"
  [& args]
  (println "Running basic tests for custom functions:")
  (println)
  (println "Getting the 5th row, 4th spot from Pascals Triangle.")
  (println (str "Value = " (my-pas-tri 5 4) ", should be 5."))
  (println "Printing the whole 4th row.")
  ;; (println (map #(my-pas-tri-recursive 4 %) (range 5)))
  (println (my-pas-tri-row 4))
  (println)
  (println (str "Merging (1 3 5 7 9) and (0 2 4 6 8 10)"))
  (println (my-merge (filter odd? (range 11)) (filter even? (range 11))))
  (println)
  (println "Merge sort for randomized (0 1 2 3 4 5 6 7 8 9 10)")
  (let [test (shuffle (range 11))]
    (println (str "Before = " test))
    (println (str "After  = " (my-merge-sort test)))
  )
  (println)
  (println "Building a BST from (0 1 2 3 4 5 6 7)")
  (println (my-build-bst (shuffle (range 8))))
  (println)
  (println "Traversing BST in order from randomized initial built BST.")
  (let [test (my-build-bst (shuffle (range 8)))]
    (println (str "Initial bst " test))
    (println (my-bst-in-order test))
  )
  (println)
  (println "Reversing (0 1 2 3 4 5 6 7 8 9 10)")
  (println (my-reverse (range 11)))
)
