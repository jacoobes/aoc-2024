(ns y2024.d8
  (:require [clojure.test :as t :refer [deftest]]))

;; PROBLEM LINK https://adventofcode.com/2024/day/8

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [grid (map vec (clojure.string/split-lines input))] 
  [(vec grid) (for [[i row] (map-indexed vector grid ) 
                     [j el]  (map-indexed vector row) 
                     :when (not= el \.)]
                     [[i j] el])]))

(defn slope [v1 v2]
  (map - v2 v1))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[grid input]]
  (let [antis (for [ant1 input
                    ant2 input 
                    :let [[pos1 sig1] ant1
                          [pos2 sig2] ant2]
                    :when (and (= sig1 sig2) (not= pos1 pos2) )  ] 
                    (let [riserun (slope pos1 pos2)] 
                      [(map - pos1 riserun) (map + pos2 riserun ) ])) ] 
    (count (into #{ } (filter #(get-in grid %) (apply concat antis))  )) ))

(defn find-antis [op pt riserun grid] 
  (reduce (fn [acc cur]  
            (if (nil? (get-in grid (vec (peek acc)) )) 
                (reduced acc)
                (conj acc (map op (peek acc) riserun) ))) [(map op pt riserun)] (range)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [[grid input]] 
  (let [antis (for [ant1 input
                    ant2 input 
                    :let [[pos1 sig1] ant1
                          [pos2 sig2] ant2 
                          #_ (println pos1 pos2 (slope pos1 pos2)) ]
                    :when (and (= sig1 sig2) (not= pos1 pos2) )  ] 
                    (let [riserun (slope pos1 pos2) ] 
                      (concat (find-antis - pos2 riserun grid ) 
                              (find-antis + pos2 riserun grid ))))] 
    (count (into #{ } (filter #(get-in grid %) (apply concat antis))))))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(def sample-data 
"............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")
(deftest sample-test
  (t/is (= 14 (solve-part-1 (generator sample-data)))))

(deftest sample-test-2
  (t/is (= 34 (solve-part-2 (generator sample-data)))))
