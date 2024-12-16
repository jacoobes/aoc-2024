(ns y2024.d15
  (:require [clojure.test :as t :refer [deftest]]))

;; PROBLEM LINK https://adventofcode.com/2024/day/15

;; Generator Logic

;; Solution Logic

;; Entry Points
(def sample-data 
"########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<")

(def offset { :north [-1 0]
              :south [1 0]
              :east  [0 1]
              :west  [0 -1] })

(defn rotate [orientation]
  (case orientation
    \^ :north \> :east \< :west \v :south ))

(defn push-block [grid orientation curpos newpos] 
    ;curpos should be adjacent to 0, newpos should be location of it
    
)

(let [[grid instrucs] (clojure.string/split sample-data #"\n\n")
       grid (mapv vec (clojure.string/split-lines grid))     
       startpos (first  (for [[i r] (map-indexed vector grid) 
                      [j c] (map-indexed vector r) 
                      :when (= \@ c)] [i j]))
       _ (println grid)] 
  (loop [grid grid
         [f & rst] instrucs
         curpos  startpos 
         result #{} ] 
    (if (not f)
        curpos
        (let [newpos (map + curpos (offset  (rotate f)))]  
          (case (get-in grid newpos) 
            \# (recur grid rst curpos)
            \O (let []  
                (recur grid rst curpos))
            \. (recur grid rst newpos)
            (recur grid rst newpos))))))

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input])

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input])

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input])

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
