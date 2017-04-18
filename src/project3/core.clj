(ns project3.core)

;; Note that the function "+'" does the same as "+" but
;; it automaticaly promotes numbers to BigInt as needed.

(defn add-pairs
  "Adds the pairs of numbers together.
    Starts at the front and leaves the last alone."
  [x]
  (if (= 1 (count x))
    x
    (cons (+' (first x) (second x)) (add-pairs (rest x)))
  )
)

(defn add-pairs-rev
  "Adds the pairs of numbers together.
    Starts at the back and leaves the first alone."
  [x]
  (reverse (add-pairs (reverse x)))
)

(defn my-pas-tri-row
  "Gets a list representing a row in Pascal's Triangle using recursion."
  [n]
  (cond
    (< n 0) nil
    (= n 0) '(1)
    :else
      (add-pairs (cons 0 (my-pas-tri-row (- n 1))))
  )
)

(defn take-or-pad
  "Returns a collection with n items taken from coll,
    padded with val if needed."
  [n val coll]
  ;; repeat and concat are lazy sequences and stop once take is satisfied
  (take n (concat coll (repeat val)))
)

(defn my-pas-tri-row-fast
  "Gets a list representing a row in Pascal's Triangle,
    optionally only generates first k values in row."
  ([n k] ;; only generates rows up to k long, saving lots of time.
    (cond
      (< n 0) nil
      (= n 0) '(1)
      :else
        (add-pairs-rev (take-or-pad
                         (+ (min k n) 1)
                         0
                         (my-pas-tri-row-fast (- n 1) k)
                       )
        )
    )
  )
  ([n] ;; If no k is specified, get whole row.
    (my-pas-tri-row-fast n (+ n 1))
  )
)

(defn my-pas-tri
  "Gets a value from Pascal's Triangle
    by first calculating the row."
  [n k]
  (if (< n k)
    nil
    (nth (my-pas-tri-row n) k)
    )
  )

(defn my-pas-tri-fast
  "Gets a value from Pascal's Triangle
    by first calculating the row using the fast method."
  [n k]
  (cond
    (< n k) nil
    ;; Since rows are symetric I can save time by dealing with the first half.
    (> k (/ n 2)) (nth (my-pas-tri-row-fast n (- n k)) (- n k))
    :else (nth (my-pas-tri-row-fast n k) k)
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
  "Inserts a value into the binary search (sub-)tree given by node."
  [x node]
  (if (= 3 (count node))
    (let [key (nth node 0) left (nth node 1) right (nth node 2)]
      (cond
        (< x key)
          (if (empty? left) ;; both nil and '() are "emtpy" by this predicate
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
  "Builds a binary search tree from a list."
  [x]
  (if (= 1 (count x))
    (bst-new-node (last x))
    (my-bst-insert (last x) (my-build-bst (butlast x)))
  )
)

(defn my-bst-in-order
  "In order traversal of a binary search tree produced by my-build-bst"
  [node]
  (if (= 3 (count node))
    (let [key (nth node 0) left (nth node 1) right (nth node 2)]
      (concat (my-bst-in-order left) (cons key (my-bst-in-order right)))
    )
    ()
  )
)

(defn my-tree-sort
  "Sorts coll using a binary search tree. Note that this removes duplicates."
  [coll]
  (my-bst-in-order (my-build-bst coll))
)

(defn my-reverse
  "Reverses coll. Returns a vector."
  [coll]
  (if (empty? coll)
    [] ;; Return an empty vector. Vector's conj side is the back
    (conj (my-reverse (rest coll)) (first coll))
  )
)

(defn my-fib
  "Recursively finds the nth fibonacci number."
  [n]
  (if (<= n 1)
    1
    (+ (my-fib (- n 1)) (my-fib (- n 2)))
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
  (println (my-pas-tri-row 4))
  (println "Timing fast method against regular for n = 500 and k = 125.")
  (print "Regular: ")
  (time (my-pas-tri 500 125))
  (print "Fast: ")
  (time (my-pas-tri-fast 500 125))
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
