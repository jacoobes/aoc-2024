(ns y2024.d11
  (:require [clojure.test :as t :refer [deftest]]))

;; PROBLEM LINK https://adventofcode.com/2024/day/11

;; Generator Logic

;; Solution Logic

;; Entry Points
(def sample-data "125 17")

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (vec (re-seq #"\d+" input)) )

;https://stackoverflow.com/a/7089841
(def trim-leading-zeros 
 (memoize  (fn [^String source]
    (loop [i 0 
           s source] 
      (if (= i (count s))
       "0"
       (if (not= \0 (.charAt s i))
        (subs s i)
        (recur (inc i)  s)) )))))

#_(trim-leading-zeros a)

(def split-number 
   (memoize (fn [digits]
    (let [half (quot (count digits) 2)] ; Calculate the midpoint
      [ (subs digits 0 half)
        (trim-leading-zeros (subs digits half))   ])))) ; Split into halves

(def year-parser (memoize (fn  [cur]
                           (str (* 2024 (parse-long cur))))))

(defn updater [c]
  (cond 
    (nil? c) 1
    :else (inc c)))

(defn update-pair [m e]
     (update m e updater))

(def blicky (fn [acc cur]
      (cond 
        (= "0" cur) (-> (update acc "1" updater)
                        (update cur  (fn [v] (if (nil? v) 0 (dec v))  ))) 
        (even? (count cur))  (let [v (split-number cur) ]
                               (-> (reduce update-pair acc v)
                                   (update cur (fn [v] (if (nil? v)  0 (dec v))  ))
                                   ))
        :else (-> (update acc (year-parser cur) updater)
                  
                  (update cur (fn [v] (if (nil? v)  0 (dec v))  ))) 
        
        )))

(let [n 5
      input (generator sample-data) ] 
  (loop [cnt 0
         result (reduce blicky {} input) ] 
      (if (= cnt n)
        (vals result)
        (let [new-map  (into {}  (->> (filter (comp pos? second) result))) 
              _ (println new-map) ] 
          (recur (inc cnt) (reduce blicky {} (->> (mapcat (fn [[k v]] (repeat v k))  new-map))))))))

(defn light-years [n input] 

  (loop [cnt 0
         result input ] 
      (println cnt)
      (if (= cnt n)
        result
        (recur (inc cnt) 
              (persistent! (reduce blicky (transient []) result))  )  )))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (count (light-years 25 input)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (count (light-years 75 input)))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question
#_(println (count (solve-part-1 (generator sample-data))))

#_(let [data (generator (slurp "input/2024/11.txt"))]
      (count (solve-part-2 data)))

(deftest sample-test
  (t/is (= 22 (solve-part-1 (generator sample-data)) )))

#_(defn -main [& args] 
  (println (count (light-years 75 (generator (slurp "input/2024/11.txt")))))  )



