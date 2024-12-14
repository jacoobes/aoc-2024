(ns y2024.d12
  (:require [clojure.test :as t :refer [deftest]]))

;; PROBLEM LINK https://adventofcode.com/2024/day/12
(def offset [[-1  0] [1  0] [0  1] [0 -1] ])

(defn neighbors [curpos grid] 
 (let [cands (map #(map + curpos %) offset)]
   (filter #(get-in grid %) cands)))

(defn candidates [visited curpath grid] 
  (->> (filter (comp not visited) (neighbors curpath grid))))

(defn bfs [grid startpos]  
  (loop [visited #{}
         queue  [startpos] 
         result #{} ] 
    (if (empty? queue)
        result
        (let [curpath (peek queue)
              cands  (->> (candidates visited curpath grid) 
                          (filter #(= (get-in grid startpos) (get-in grid  %)))) ]  
          (cond 
            (= (get-in grid startpos) (get-in grid curpath)) (recur (conj visited, curpath) 
                                                                    (into  (pop queue) cands) 
                                                                    (conj result  curpath))
            :else (recur (conj visited, curpath) (into queue cands) result))))))

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [grid (mapv vec (clojure.string/split-lines input)) ]  
    grid))


(defn candidates [visited curpath grid] 
  (filter (comp not visited) 
          (neighbors curpath grid)))


(defn get-perimeter [grid region] 
  (let [vregion (vec region)
        region-id (get-in grid (first vregion)) 
        num-nearby-pts (apply + (map (fn [pos]   
                                 (let [adjacents (map #(map + pos %) offset)] 
                                   (count (filter #(not= region-id (get-in grid %)) adjacents))))  vregion))]
        num-nearby-pts))

(defn get-regions [input] 
  (loop [[hd & rst] (for [i (range 0 (count input)) 
                          j (range 0 (count (first input)))]
                       (list i j)) 
         result #{}] 
    (let [walked (bfs input hd) ]
      (if hd 
        (recur (filter (comp not walked) rst) (conj result walked))
        result))))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (let [islands (get-regions input)]
    (->>  (map (fn [region]  
                 (let [v {:area (count region) 
                          :perimeter (get-perimeter input region)} ] 
                   v)) islands)
          (map #(* (:area %) (:perimeter %)))
          (apply +))))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input])

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(def sample-data 
"AAAA
BBCD
BBCC
EEEC")

(def sample-data-2
"RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")

(def sample-data-3
"OOOOO
OXOXO
OOOOO
OXOXO
OOOOO")

(def sample-data-4
"OBBBBL
OBBBBB
OBBBBB
OJXXXJ")

(def sample-data-5
"OBABBB
OBBABB
OBBBBB
OXXXXX")

(def sample-data-6 
"OBBBBB
 OOBBBB
 OBOBOB
 OXXOXX")

(deftest sample-test
  (t/is (= 140 (solve-part-1 (generator sample-data)))))

(deftest sample-test-2
  (t/is (= 1930 (solve-part-1 (generator sample-data-2)))))

(deftest sample-test-3
  (t/is (= 772 (solve-part-1 (generator sample-data-3)))))

(deftest sample-test-4
  (t/is (= 300 (solve-part-1 (generator sample-data-4)))))

(deftest sample-test-5 
  (t/is (= 4  (count (get-regions (generator sample-data-5))))))


