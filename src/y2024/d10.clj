(ns y2024.d10
  (:require [clojure.test :as t :refer [deftest]]))

;; PROBLEM LINK https://adventofcode.com/2024/day/10

<<<<<<< HEAD
=======

(def sample-data-1 
"0123
1234
8765
9876")

(def grid 
  (vec (map #(vec (map (comp parse-long  str) %) ) (clojure.string/split-lines sample-data-1)))) 
grid
(def offset [[-1 0]
              [1 0]
              [0 1]
              [0 -1]])

(defn neighbors [curpos grid] 
 (let [cands (map #(map + curpos %) offset)]
   (filter #(get-in grid %) cands)))

(defn candidates [visited curpath grid] 
  (->> (filter (comp not visited) (neighbors  curpath grid))
       (filter #(= (inc (get-in grid curpath)) (get-in grid (vec %))))))

(defn find-9s [startp]  
  (loop [visited #{}
         queue [startp]
         count9 0 ] 
    (if (empty? queue)
        count9
        (let [curpath (first queue)
              cands  (candidates visited curpath grid)
              _ (println queue (get-in grid curpath)) ]  
          (cond 
            (= 9 (get-in grid curpath)) (recur (conj visited, curpath) (rest queue ) (inc count9))
            (empty? cands) (let [ _ (println cands)
                                  new-q (map #(candidates visited % grid) queue)
                                  _ (println "oldq" queue "newq " new-q ) ] 
                                  (recur (conj visited, curpath) [] count9)) 
            :else (recur (conj visited, curpath) (into (rest queue), cands) count9)
            )))
      )
    )
 
(println (find-9s [0 0])) 
#_(defn- dfs2
  [grid goal]
  (fn search
    [path visited]
    (let [current (peek path)]
      (if (= goal current)
        [path]
        (->> (->> (filter (comp not visited) (neighbors (peek path) grid))
                          (filter #(= (inc (get-in grid (peek path))) (get-in grid (vec %)) ))) 
             (remove visited)
             (mapcat #(search (conj path %) (conj visited %))))))))

#_(defn findpath
  "Returns a lazy sequence of all directed paths from start to goal
  within graph."
  [graph start goal]
  ((dfs2 graph goal) [start] #{start}))
#_(count (findpath grid '(0 0)  [3 0])) 
#_(dfs #{} [[0 2]]) 
>>>>>>> 6e448a0 (d10)
;; Generator Logic

;; Solution Logic

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
<<<<<<< HEAD
  [input]
  (vec (map #(vec (map (comp parse-long str) %)) 
            (clojure.string/split-lines input))))

(def input (generator "0123
1234
8765
9876"))

(def directions [[-1 0]
                 [1 0]
                 [0 1]
                 [0 -1] ])

(defn neighbors [input curpos] 
  (map  #(map + curpos %)  directions))


#_   (->> (filter #(get-in input (vec %)) (neighbors input curpos))   
                              (filter (comp not visited)) 
                              (map vec))
(loop [visited #{}
       path [[0 0]]] 
    (if (= 9 (get-in input (peek path)))  
      path
      ))
=======
  [input])
>>>>>>> 6e448a0 (d10)

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input])

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input])

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
