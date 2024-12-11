(ns y2024.d10
  (:require [clojure.test :as t :refer [deftest]]))

;; PROBLEM LINK https://adventofcode.com/2024/day/10

(def sample-data-1 
"89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")
(defn generator [input] 
  (vec (map #(vec (map (comp parse-long  str) %) ) (clojure.string/split-lines input))))

(def offset [[-1 0]
              [1 0]
              [0 1]
              [0 -1]])

(defn neighbors [curpos grid] 
 (let [cands (map #(map + curpos %) offset)]
   (filter #(get-in grid %) cands)))

(defn candidates [visited curpath grid] 
  (->> (filter (comp not visited) (neighbors  curpath grid))
       (filter #(= (inc (get-in grid curpath)) (get-in grid (vec %))))))

(defn find-9s [grid startp]  
  (loop [visited #{}
         queue [startp]
         count9 0] 
    (if (empty? queue)
        count9
        (let [curpath (first queue)
              cands  (candidates visited curpath grid) #_ (println queue (get-in grid curpath)) ]  
          (cond 
            (= 9 (get-in grid curpath)) (recur (conj visited, curpath) (rest queue ) (inc count9))
            (empty? cands) (recur (conj visited, curpath) (rest queue) count9)  
            :else (recur (conj visited, curpath) (into (rest queue), cands) count9) )))))

 (defn find-uniques [grid startp]
   (loop [visited #{}
         queue [startp]
         count9 0] 
    (if (empty? queue)
        count9
        (let [curpath (first queue)
              cands  (candidates visited curpath grid) #_ (println queue (get-in grid curpath)) ]  
          (cond 
            (= 9 (get-in grid curpath)) (recur #{(last queue)} [startp] (inc count9))
            (empty? cands) (recur (conj visited, curpath) [] (inc count9) )  
            :else (recur (conj visited, curpath) (into (rest queue), cands) count9) )))))


(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (let [zeros  (for [[i row ] (map-indexed vector input)
                      [j el] (map-indexed vector row)
                      :when (zero? el) ] [i j])] 
    (apply + (map (partial find-9s input) zeros))))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [zeros  (for [[i row ] (map-indexed vector input)
                      [j el] (map-indexed vector row)
                      :when (zero? el) ] 
                  [i j])] 
    (apply +  (map (partial find-uniques input) zeros))))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 36 (solve-part-1 (generator sample-data-1)) )))

(deftest sample-test
  (t/is (= 81 (solve-part-2 (generator sample-data-1)) )))

