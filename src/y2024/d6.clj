(ns y2024.d6
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as string]))


;; PROBLEM LINK https://adventofcode.com/2024/day/6

;; Generator Logic

;; Solution Logic

;; Entry Points

(def sample-data-1 
"....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [grid-map (vec (map vec (string/split-lines input))) 
        position (first (keep-indexed #(when (pos? %2) [%1 %2] ) (map #(.indexOf % \^) grid-map)))  ]
    [grid-map position] ))

(def offset { :north [-1 0]
              :south [1 0]
              :east  [0 1]
              :west  [0 -1] })

(defn rotate [orientation]
  (case orientation
    :north :east
    :east  :south
    :south :west
    :west :north))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (let [[grid-map position]  input ]
  (loop [orientation :north 
         cur-position position
         walked #{cur-position}] 
    (let [new-pos (vec (map + (offset orientation) cur-position))
          ch (get-in grid-map (vec new-pos))]
      (if ch
        (if (= ch \#)
          (recur (rotate orientation) cur-position walked) 
          (recur orientation new-pos (conj walked new-pos)))
        (count walked) )))))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [[grid-map position]  input ]
    grid-map
  ))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 41 (solve-part-1 (generator sample-data-1)) )))
