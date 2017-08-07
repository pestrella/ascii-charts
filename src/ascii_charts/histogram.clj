;; Copyright (c) 2017 Paolo Estrella
(ns ascii-charts.histogram)

(defn max* [data]
  (apply map max data))

(defn max-len [coll]
  (->> (map str coll)
       (map count)
       (apply max)))

(defn double-space [s]
  (apply str (map #(str " " %) s)))

(defn mid [x y]
  (let [[mn mx] (if (< x y) [x y] [y x])]
    (+ mn (/ (- mx mn) 2))))

(defn quarters [n]
  (let [q2 (mid 0 n)
        q1 (mid 0 q2)
        q3 (mid q2 n)
        q4 n]
    (map int [q1 q2 q3 q4])))

(defn char-line [c length]
  (->> (repeat c)
       (take length)
       (apply str)))

(defn fixed-len [s length & [align]]
  (let [chars (take length s)]
    (loop [c (if (= :right align)
               (into [] chars)
               chars)]
      (if (< (count c) length)
        (recur (conj c " "))
        (apply str c)))))

(def ^:dynamic *histo-height* 12)

(defn histo [data]
  (let [max-y (second (max* data))
        scale (/ *histo-height* max-y)]
    (->> data
         (map (fn [[_ y]] (fixed-len (char-line "*" (* y scale)) *histo-height*))))))

(defn x-axis [data]
  (let [x-vals (map first data)
        y-vals (map second data)
        x-label-offset (max-len x-vals)
        y-label-offset (inc (max-len y-vals))
        x-labels (->> (map #(fixed-len (str %) x-label-offset :right)
                           x-vals)
                      (apply map str)
                      (map double-space))
        x-axis-len (count (first x-labels))]
    (->> (conj x-labels (char-line "-" x-axis-len))
         (map #(fixed-len % (+ x-axis-len y-label-offset))))))

(defn print-histo [data]
  (let [y-vals (map second data)
        max-y (second (max* data))
        y-label-offset (inc (max-len y-vals))
        y-labels (into {} (map vector (quarters *histo-height*) (quarters max-y)))
        h (histo data)
        chart (map-indexed
               (fn [i s]
                 (if-let [label (y-labels (- *histo-height* i))]
                   (str (fixed-len (str label) y-label-offset) s)
                   (str (fixed-len " " y-label-offset) s)))
               (->> h
                    (apply map str)
                    (map double-space)))]

    (for [row (-> (into [] chart)
                  (into (x-axis data)))]
      (println row))))

(comment
  (print-histo [[0 123]
                [25 32]
                [50 85]
                [75 52]
                [100 102]
                [125 44]]))
