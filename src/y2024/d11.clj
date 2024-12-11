(ns y2024.d11
  (:require [clojure.test :as t :refer [deftest]]))

;; PROBLEM LINK https://adventofcode.com/2024/day/11

;; Generator Logic

;; Solution Logic

;; Entry Points
(def sample-data "125 17")

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (vec (re-seq #"\d+" input)) )


(def parse-halves 
  (memoize (comp str parse-long))) ;ensure that each

(def split-number 
   (memoize (fn [num]
    (let [digits num ; Convert the number to a string
          half  (quot (count digits) 2)] ; Calculate the midpoint
      [(subs digits 0 half) (subs digits half)])))) ; Split into halves

(def year-parser (memoize (fn  [cur]
                           (str (* 2024 (parse-long cur))))))

(def blicky (fn [acc cur]
              (cond 
                (= "0" cur) (conj acc "1")
                (even? (count cur))  (let [v (split-number cur) #_ (println cnt ) 
                                           #_ (println (map parse-halves v)) ]
                                           (into acc (map parse-halves) v))
                :else (conj acc (year-parser cur)))))

(defn light-years [n input] 
  (loop [cnt 0
         result input] 
      (if (= cnt n)
        result
         (recur (inc cnt) 
              (reduce blicky [] result) )  )))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (count (light-years 25 input)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (count (light-years 75 input)))
;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question
#_(println (count (solve-part-1 (generator sample-data))))
(deftest sample-test
  (t/is (= 22 (solve-part-1 (generator sample-data)) )))


