(ns six-handshakes.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def dataset (slurp (io/resource "dataset.txt")))
(def parsed-data (map (fn [line] (string/split line #" "))
                      (string/split-lines dataset)))
(def connections (apply merge-with
                        (partial apply conj)
                        (map (fn [[from to]] {from [to], to [from]}) parsed-data)))
(def handshakes
  (pmap (fn [[source targets]]
          (loop [i 1
                 visited (set (conj targets source))
                 current (set targets)
                 result {}]
            (if (empty? current)
              result
              (recur (inc i)
                     (apply conj visited current)
                     (apply disj (set (flatten (map #(get connections %) current))) (apply conj visited current))
                     (assoc result i (count current))))))
        connections))

(defn mean [coll]
  "Calculates mean (average) value for the sequence"
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(defn median [coll]
  "Calculates median value for the sequence"
  (let [sorted (sort coll)
        count (count sorted)
        half (quot count 2)]
    (if (odd? count)
      (nth sorted half)
      (let [bottom (dec half)
            bottom-val (nth sorted bottom)
            top-val (nth sorted half)]
        (mean [bottom-val top-val])))))

(defn -main []
  (let [joined (apply merge-with conj (into {} (for [i (range 1 9)] [i []])) handshakes)]
    (println "Total values:" (apply merge-with + handshakes))
    (println "Mean values:" (into {} (map (fn [[source values]] [source (float (mean values))]) joined)))
    (println "Median values:" (into {} (map (fn [[source values]] [source (float (median values))]) joined)))))
