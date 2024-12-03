(ns y2024.d3
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as string]
            [clojure.math :as math]
            ))

(defn extract-data [matches]
  (let [captured (first matches ) ] 
      (->> (cond (string/starts-with? captured "mul") {:instruction "mul" :args (read-string (get matches 2))} 
                 (string/starts-with? captured "don't") {:instruction "don't" :args []}
                 (string/starts-with? captured"do") {:instruction "do" :args []} )) ))

;; PROBLEM LINK https://adventofcode.com/2024/day/2
(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> (map extract-data 
              (re-seq #"(mul(\(\d+,\d+\)))|(do\(\))|(don't\(\))" input))
       (filter #(every? (fn [e] (<= (count (str e)) 3)) (:args % )))))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (->> (map :args input)
       #_(filter #(every? (fn [e] (<= (count (str e)) 3) )  % ))
       (map #(apply * %))
       (reduce +)))



(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  #_(println input)
  (loop [[head & rst] input
         do-mul? true 
         res []] 
    (if head 
     (cond (= (:instruction head) "don't") (recur rst false res)
           (= (:instruction head) "do") (recur rst true res)
           (= (:instruction head) "mul") (if do-mul? (recur rst do-mul? (conj res (apply * (:args head)))) 
                                                     (recur rst do-mul? res))
           :else head) 
     (apply + res ) )))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question
(def sample-data-1 
"xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
(def sample-data-2
"xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(deftest sample-test-1
  (t/is (= 161 (solve-part-1 (generator sample-data-1)))))

; not 399, 503, 386, 313 , 430
(deftest sample-test-2
  (t/is (= 48 (solve-part-2 (generator sample-data-2)))))
