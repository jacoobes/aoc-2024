(ns y2024.d10
  (:require [clojure.test :as t :refer [deftest]]))

;; PROBLEM LINK https://adventofcode.com/2024/day/10

;; Generator Logic

;; Solution Logic

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
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
