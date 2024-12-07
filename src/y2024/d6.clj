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
(defn is-rect? [points]
  
  )
(defn walk-out-maze [input]
  (let [[grid-map position] input]
    (loop [orientation :north 
           cur-position position
           walked #{cur-position} ] 
      (let [new-pos (vec (map + (offset orientation) cur-position))
            ch (get-in grid-map (vec new-pos)) 
            #_ (println walked new-pos) ]
        (if ch 
          (cond (= ch \#) (recur (rotate orientation) cur-position   walked) 
                 :else (recur orientation new-pos  (conj walked new-pos)))
          walked)))))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (let [[grid-map position]  input
        positions-walked (walk-out-maze input)]
    (count positions-walked )))


(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [[grid-map position]  input
        walked (solve-part-1 input)
        all-grids (for [[i r]  (map-indexed vector grid-map)
                        [j c]  (map-indexed vector r) 
                        :when (not= c \#)]
                        [(assoc-in grid-map [i j] \#) position] )
        ]
    (->> (map walk-out-maze all-grids)
         (filter nil?)
         count)  
    ))

(= [6 4] [6 4])
(let [[grid-map position]  (generator sample-data-1) 
       walked (walk-out-maze [grid-map position])
       all-grids (for [[i r]  (map-indexed vector grid-map)
                       [j c]  (map-indexed vector r) 
                       :when  (and (not= c \#) (not= position [i j]) (some? (walked [i j]))) ]
                       [(assoc-in grid-map [i j] \o) position] ) 
      _ (clojure.pprint/pprint (nth all-grids 15)) ] 
   (get all-grids 4)
  ) 

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 41 (solve-part-1 (generator sample-data-1)) )))
(deftest sample-test-2
  (t/is (= 6 (solve-part-2 (generator sample-data-1)) )))
