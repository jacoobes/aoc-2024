(ns y2024.d9
  (:require [clojure.test :as t :refer [deftest]]))

;; PROBLEM LINK https://adventofcode.com/2024/day/9
(def sample-data "2333133121414131402")

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (vec (->> (partition 2 2 nil (map parse-long (re-seq #"\d" input))) 
       (map-indexed vector)
       #_(map (fn [[idx filedata]]
              { :id idx 
                :used (repeat (first filedata) idx) 
                :free (repeat (or (second filedata ) 0) -1)   }))))  
  
  )

(partition 2 2 nil  (map parse-long (re-seq #"\d" "12345")))
(peek (generator  sample-data) ) 

(defn contiguous [memory]
  (let [memory-layout (map (comp flatten (juxt :used  (comp flatten reverse :free))) memory)
        #_ (println memory-layout)
        ]
     (take-while #(not= -1 %)  (apply concat memory-layout))  
    ))



#_(contiguous-memory? [{:id 0, :used '(0 0), :free '(8 9 9)} {:id 1, :used '(1 1 1), :free '(8 8 8)} {:id 2, :used '(2), :free '(-1 -1 -1)}])
#_(and (not-empty result) (every? #(every? pos? %) (map :free result))) ;contiguous sequence of a number? 
; (>= (count result)  (count back)) (contiguous (conj (pop result ) (peek back)))
(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
   (loop [back input 
         [hd & rst2 :as front] input
          result [] ]
      (if (>= (count result)  (count back))      
        (if (even? (count input)) 
             (reduce + (map-indexed * (contiguous (conj (pop result ) (peek back)))))     
             (contiguous result) #_(this branch is broken lol) ) 
        (cond 
          (empty? (:used (peek back))) ;if back memory has no more used
              (recur (pop back) front result) 
          (some #{-1} (:free (peek result))) ;if result memory stiul has free
              (recur (conj (pop back) (update (peek back) :used butlast))
                     front 
                     (update result (dec (count result)) 
                                    (fn [blk] (let [freeblk (:free blk) #_ (println blk) ]
                                                (if (= (:id blk) (:id (peek back)))  
                                                  blk
                                                  (assoc blk :free (take (count freeblk) (cons (:id (peek back)) freeblk)) ))
                                                ))))
          :else (recur back rst2 (conj result hd))
          ))))
#_(contiguous (solve-part-1 (generator "12345")))  
interleave
(defn interleave-when [pred col1 col2]
  (loop [col1 col1
        col2 col2
        result []] 
      (if (empty? col2)
          result
          (cond 
            (pred (first col1)) (recur (rest col1) (rest col2) (conj result (first col2)))   
            :else               (recur (rest col1) (rest col2 ) result)))
    ))
(def fileformat (vec "00...111...2...333.44.5555.6666.777.888899"))
fileformat
(println (interleave-when #(and (= \. %)) fileformat (reverse fileformat))) 

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  
  )

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 1928 (solve-part-1 (generator sample-data )))))


(deftest sample-test-1
  (t/is (= 1928 (solve-part-1 (generator "12345")))))

(deftest sample-test-2
  (t/is (= 134 (solve-part-1 (generator "101010111010111")))))

(deftest sampletest3
  (t/is (= 0123456 (solve-part-1 (generator "1010101110101113")))))
