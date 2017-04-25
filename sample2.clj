; Let's look at the "hello world job" ie word count.
(def input [
           [1 "hadoop map-reduce explained"]
           [2 "with clojure map, reduce and mapcat"]
           [3 "using the world count example"]])

(def output [
            ["and" 1]["clojure" 1]["count" 1]
            ["example" 1]["explained" 1]["hadoop" 1]
            ["map" 2]["mapcat" 1]["reduce" 2]["the" 1]["using" 1]
            ["with" 1]["world" 1]])

; The two main customisation points are :
(declare my-mapper my-reducer)

; And at the end, we should have :
(= (mapreduce input my-mapper my-reducer) output)



; First, the mapper behavior for word count :
(= (my-mapper [1 "hadoop map-reduce explained"])
   [["hadoop" 1] ["map" 1] ["reduce" 1] ["explained" 1]])

(defn my-mapper [[k v]]
  (map #(vector % 1)(re-seq #"\w+" v)))



; Second, the reducer behavior for word count
(= (my-reducer ["map" [1 1]])
   [["map" 2]])

(defn my-reducer [[word list-occurences]]
  [[word (reduce + list-occurences)]])



; And finaly, the mapreduce implementation for single node
(defn shuffle-sort [kvs]
  (->> kvs
       (sort-by first)
       (partition-by first)
       (map #(vector (first (first %)) (map second %)))))

(defn mapreduce [kvs mapper reducer]
  (->> kvs
       (mapcat mapper)
       shuffle-sort
       (mapcat reducer)))
