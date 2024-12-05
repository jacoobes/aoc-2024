(ns y2024.d5
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as string]))

(def sample-data-1 
"47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defn into-graph [acc col] 
  (update acc (first col) #(if (nil? %) [(second col)] (conj % (second col) ))))

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [[rules seqs] (string/split input #"\n\n")
      rules (reduce into-graph {} (->> (string/split-lines rules) 
                                       (map #(string/split % #"\|")))) 
      seqs (vec (->> (map #(read-string (str "[" % "]")) (string/split-lines seqs)) (map #(map str %)))  )] 
    [rules seqs])) 
  
(defn check-valid-seq [rules myseq] 
  [myseq (reduce 
      (fn [acc col] 
        (let [prev (peek acc)
              choices (rules prev) ]
          (conj acc (some #{col} choices)))) 
      [(first myseq)] (rest myseq))])

(defn solve-part-1 [input]
  (let [[rules,seqs] input]
    (->>  (map (partial check-valid-seq rules) seqs)
          (filter #(every? some?  (second %)))
          (map second)
          (map #(->> (get %  (int (/ (count %) 2))) parse-long ))
          (apply +))))

(defn solve-part-2 
  "tHE SOLUTIon to part 2. Will be called with the result of the generator"
[input]
 (let [[rules, seqs]  input
       invalid (map first (remove #(every? some? (second %)) (map (partial check-valid-seq rules) seqs))) ]  
      (->> (map #(sort (fn [e1 e2] (cond (some #{e2} (rules e1)) -1 ;if e1 precedes e2
                                         (some #{e1} (rules e2)) 1 ;if e1 procedes e2
                                         :else 0)) %) invalid)
           (map vec)
           (map #(->> (get %  (int (/ (count %) 2))) parse-long))
           (apply +))))
#_(solve-part-2 (generator sample-data-1))


(deftest sample-test-1
  (t/is (= 143 (solve-part-1 (generator sample-data-1)))))

(deftest sample-test-2
  (t/is (= 123 (solve-part-2 (generator sample-data-1)))))


