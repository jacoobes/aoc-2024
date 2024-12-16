(ns y2024.d14
  (:require [clojure.test :as t :refer [deftest]]))

;; PROBLEM LINK https://adventofcode.com/2024/day/14

;; Generator Logic

;; Solution Logic

;; Entry Points


(defn wrap-num [start end]
  (cond 
    (neg? start) (+ start end)
    (> start end) (- start end)
    (= start end) 0 
    :end start))

(def sample-data 
"p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3")

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> (clojure.string/split-lines input)
       (map #(->> (rest (first (re-seq #"p=(\d+,\d+) v=(.?\d+,.?\d+)" %)))  
                  (map (fn [e] (read-string (str "["  e  "]"))))))
       (map (fn [[p v]] {:p p  :v v}))))

(defn simulation [pt vel secs wide tall] 
    (reduce (fn [acc cur-pt]
                (let [initial-translation (mapv + (peek acc) vel) #_ (println initial-translation) ]
                       (conj acc [(wrap-num (first initial-translation) wide) 
                                  (wrap-num (second initial-translation) tall)]))) [pt] (range 0 secs)))

(generator sample-data)

(defn quadrants [wide tall] 
    (let [h-half (dec (int  (/ wide 2))) 
          v-half (dec (int (/ tall 2)))   ]
        { :q1 (map vector  [0,0]   [h-half  v-half]) 
          :q2 (map vector  [(- (dec wide)  h-half) 0] [(dec wide)  v-half])
          :q3 (map vector  [0  (- (dec tall)  v-half)] [h-half (dec tall)])
          :q4 (map vector  [(- (dec wide)  h-half) (- (dec tall)  v-half) ] [(dec wide) (dec tall)]) }))


(defn location [{:keys [q1 q2 q3 q4]} [x y]] 
   (cond 
      (and (<= (ffirst q1) x (last (first q1))) (<= (first (second q1)) y (second  (second q1)) )) :q1  
      (and (<= (ffirst q2) x (last (first q2))) (<= (first (second q2)) y (second  (second q2)) )) :q2  
      (and (<= (ffirst q3) x (last (first q3))) (<= (first (second q3)) y (second  (second q3)) )) :q3  
      (and (<= (ffirst q4) x (last (first q4))) (<= (first (second q4)) y (second  (second q4)) )) :q4 ))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input] 
  (let [wide 101 
        tall 103
        seconds 100
        quads (quadrants wide tall)
        paths (->> (map (fn [{ pt :p  v :v  }] 
                            (simulation pt v seconds wide tall))  input)
                   (map peek))
        grouped-pts (group-by (partial location quads) paths) ]
        (apply * (map count  (vals  (dissoc grouped-pts nil)) ))))

(defn append-to-file
  "Uses spit to append to a file specified with its name as a string, or
   anything else that writer can take as an argument.  s is the string to
   append."     
  [file-name s]
  (spit file-name s :append true))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [wide 101 
        tall 103
        filename "d14p2.txt"
        paths (->> (map (fn [{ pt :p  v :v  }] (simulation pt v 399 wide tall))  input)) 
        _ (println (count  (first paths))  )
        spaths (set paths)
        _ (spit filename "") ]
        (reduce (fn [acc cr]
            (let [ #_ (println (count paths) (distinct paths) ) ]
                    (do (append-to-file filename
                          (str "\n\n---------------------------------------------------" cr "---------------------------------------------------\n\n" 
                            (with-out-str 
                                (clojure.pprint/pprint 
                                    (for [i (range 0 tall)] 
                                        (clojure.string/join (map-indexed (fn [j _] (if (spaths [j i]) "🟩" "  ")) (range 0 wide))))))))
                        (reduced cr)))) (range 1 8000))))
        
;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

#_(deftest sample-test
  (t/is (= 12 (solve-part-1 (generator sample-data)))))
