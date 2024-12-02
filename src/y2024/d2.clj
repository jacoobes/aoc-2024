(ns y2024.d2
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as string]
            [clojure.math :as math]
            ))

(defn diffs [xs]
  (map - xs (rest xs)))

(defn strictly-inc? [xs] 
  (every? pos? xs) )

(defn strictly-dec? [xs] 
  (every? neg? xs) )

;; PROBLEM LINK https://adventofcode.com/2024/day/2
(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (map #(read-string (str "[" % "]") ) 
       (string/split-lines input)))

(defn issafe? [arr]
  (and (every? #(<= 1 (abs %) 3) arr)
       (or (strictly-inc? arr) (strictly-dec? arr))))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (->> input
       (map diffs)
       (filter issafe?)
       (count)))

(defn strictly-inc? [xs] 
  (every? pos? xs))

(defn strictly-dec? [xs] 
  (every? neg? xs))

(defn perms [c] 
  (map #(concat (take % c) (drop (inc %)  c))
     (range (count c))))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [safe-reports 
        (count 
         (filter 
          (fn [report]
            (or 
             ;; Original report is safe
             (issafe? (diffs report))
             ;; Check if removing any single level makes it safe
             (some 
              #(issafe? (diffs %)) 
              (map #(concat (take % report) (drop (inc %) report)) 
                   (range (count report))))))
          input))]
    safe-reports))


;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(def sample-data 
"7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")


(deftest sample-test
  (t/is (= 2 (solve-part-1 (generator sample-data)))))

; not 399, 503, 386, 313 , 430
(deftest sample-test-2
  (t/is (= 4 (solve-part-2 (generator sample-data)))))
