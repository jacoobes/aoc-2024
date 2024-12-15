(ns y2024.d13
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as string]
            [clojure.math.numeric-tower :as math]))

;; PROBLEM LINK https://adventofcode.com/2024/day/13

(def sample-data 
"Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

(defn parse-line [line]
    (re-find #"(.+):\s+X.(\d+),\s+Y.(\d+)" line))
(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
   (->> (clojure.string/split input #"\n\n")
       (map (fn [ent]  
                (let [[f s t] (map parse-line (clojure.string/split-lines ent) )] 
                    { :btna     [(parse-long  (get f 2)) (parse-long  (get f 3))]   
                      :btnb     [(parse-long  (get s 2)) (parse-long  (get s 3))]    
                      :solution [(parse-long  (get t 2)) (parse-long  (get t 3)) ] })))))

(defn det2x2 [mat]
 (- (* (get-in mat [0 0]) (get-in mat [1 1])) 
    (* (get-in mat [0 1]) (get-in mat [1 0]))) )


(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
   (->> (map (fn [ent] 
             (let [detsys (det2x2 (mapv vector (:btna ent) (:btnb ent)))
                   solb   (det2x2 (mapv vector (:btna ent) (:solution ent))) 
                   sola   (det2x2 (mapv vector (:solution ent) (:btnb ent))) ]
                (+ (* 3  (/ sola detsys)) (/ solb detsys)) )) input)
        (filter int?)
        (apply +)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
    (->> (map (fn [ent] (update ent :solution #(mapv (partial + 10000000000000)  %)))  input)
         (solve-part-1)))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question
(deftest sample-test
  (t/is (= 480 (solve-part-1 (generator sample-data)))))
(deftest sample-test
  (t/is (= 875318608908 (solve-part-2 (generator sample-data)))))
