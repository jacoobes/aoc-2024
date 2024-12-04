(ns y2024.d4
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as string]))

;; PROBLEM LINK https://adventofcode.com/2024/day/4

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (mapv #(apply vector %) (string/split-lines input)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [grid]
  (let [possibilities (for [[i rw]  (map-indexed vector grid) 
                            [j ch]  (map-indexed vector rw)
                            :when (= ch \X)
                            :let [vertical    [(take 4 (range i -1 -1)) (take 4 (range i (count grid)))] ;top bottom  
                                  horizontal  [(range j  (- j 4) -1)   (range j (+ j 4) 1)]  ]] ;left, right
                            (concat
                             (map (fn [js]    (map #(get-in grid [i %]) js)) horizontal) 
                             (map (fn [is]    (map #(get-in grid [% j]) is)) vertical)
                             [(map (fn [& args] (get-in grid args)) (first vertical) (first horizontal))] #_topleft 
                             [(map (fn [& args] (get-in grid args)) (first vertical) (second horizontal))] #_topright
                             [(map (fn [& args] (get-in grid args)) (second vertical) (first horizontal))] #_bottomleft
                             [(map (fn [& args] (get-in grid args)) (second vertical) (second horizontal))]  #_bottomright))] 
    (count (filter #(= (apply str %) "XMAS") (mapcat identity possibilities)))))
#_(solve-part-1 (generator sample-data-1))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [grid]
  (let [possibilities (for  [[i rw]  (map-indexed vector grid) 
                            [j ch]  (map-indexed vector rw)
                            :when (= ch \A)
                            :let [leftcross  [[(dec i) (inc j)] [i j] [(inc i) (dec j)]]
                                  rightcross [[(dec i) (dec j)] [i j] [(inc i) (inc j)]]] ]
                            [(apply str (map (partial get-in grid )leftcross)) (apply str (map (partial get-in grid) rightcross))])
        cross #{"SAM" "MAS"} ]
      (count (filter #(every? cross  %) possibilities) )))
#_(solve-part-2 (generator sample-data-1))

(def sample-data-1 "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")






#_[(range j  (- j 3) -1) (range j (+ j 3) 1)]
(deftest sample-test
  (t/is (= 18 (solve-part-1 (generator sample-data-1)))))
(deftest sample-test-2
  (t/is (= 9 (solve-part-2 (generator sample-data-1)))))
