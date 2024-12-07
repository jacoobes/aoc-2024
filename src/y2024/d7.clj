(ns y2024.d7
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]
            ))
;; PROBLEM LINK https://adventofcode.com/2024/day/7

;; Generator Logic

;; Solution Logic

;; Entry Points

(def sample-data 
  "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> (string/split-lines input)
       (map #(clojure.string/split % #":"))
       (map #(vector (parse-long (first %)) (read-string (str "[" (second % ) "]") )))))
(defn merge-adj [e1 e2] 
  (parse-long (str e1 e2)) )



(def ops [+ * ] )
(def ops-2 [+ * merge-adj])

(defn crunch-equations [ops equation] 
(let [[testnum nums] equation
      cart (apply combo/cartesian-product  (repeat (dec (count nums)) ops )  ) ]
    [testnum (->> (map (fn [c] [(vec c) nums]) cart)
                  (map (fn [[stack nums]]  
                         (loop [[op & nxtop] stack
                                [m & nxtnum] nums
                                result 0]
                            (cond 
                              (not op) result
                              (zero? result)  (recur nxtop (next nxtnum ) (op m (first nxtnum)))
                              :else (recur nxtop nxtnum (op result m)))))))] ))
(defn sum-crunches [arr]
  (->>(filter #(some #{(first %)} (second %) ) arr )
      (map first)
      (apply +)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (let [cruncher (partial crunch-equations ops)] 
    (->> (map cruncher input)
         (sum-crunches))))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [cruncher (partial crunch-equations ops-2)] 
    (->> (map cruncher input)
         (sum-crunches))))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test-1
  (t/is (= 3749 (solve-part-1 (generator sample-data)) )))
