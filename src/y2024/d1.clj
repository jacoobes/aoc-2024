(ns y2024.d1
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as string]))

;; PROBLEM LINK https://adventofcode.com/2024/day/1

;; Generator Logic

;; Solution Logic

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [cols (->> (string/split-lines (string/trim input))
                  (map #(re-seq #"\d+" %))
                  (apply map vector)
                  (map (fn [arr]  (map read-string arr)))) ] 
    cols))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (->> input 
       (map sort) 
       (apply map (comp abs -)) 
       (reduce +)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [[left right] input
        [fleft fright] (map frequencies input)
        #_ (println fleft fright left right)
        ]
    (->> (map (fn [i] (* i (get fright i 0)) ) left) 
         (apply +)) ))


;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(def sample-data 
"
3   4
4   3
2   5
1   3
3   9
3   3
")


(deftest sample-test-pt-2
  (t/is (= '(2 1 0 1 2 5) (solve-part-1 (generator sample-data) ) )))

(deftest sample-test-pt-2
  (t/is (= '(9 4 0 0 9 9) (solve-part-2 (generator sample-data))  )))

