;; Copyright (c) 2017 Paolo Estrella
(ns ascii-charts.histogram)

(defn max* [data]
  (apply map max data))

(defn char-len [x]
  (-> (str x) (count)))

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

(defn fixed-len [s length]
  (loop [c (take length s)]
    (if (< (count c) length)
      (recur (conj c " "))
      (apply str c))))

(def ^:dynamic *histo-height* 12)

(defn histo [data]
  (let [max-y (second (max* data))
        scale (/ *histo-height* max-y)]
    (->> data
         (map (fn [[_ y]] (fixed-len (char-line "*" (* y scale)) *histo-height*))))))

(defn x-axis [data]
  (let [x-vals (map first data)
        max-x (first (max* data))
        max-y (second (max* data))
        x-label-offset (char-len max-x)
        y-label-offset (inc (char-len max-y))
        x-labels (->> (map #(fixed-len (str %) x-label-offset)
                           x-vals)
                      (apply map str)
                      (map double-space))
        x-axis-len (count (first x-labels))]
    (->> (conj x-labels (char-line "-" x-axis-len))
         (map #(fixed-len % (+ x-axis-len y-label-offset))))))

(defn print-histo [data]
  (let [max-x (first (max* data))
        max-y (second (max* data))
        x-label-offset (inc (char-len max-x))
        y-label-offset (inc (char-len max-y))
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
  (print-histo [[1405 123]
                [1410 32]
                [1415 85]
                [1420 52]
                [142 102]
                [1430 44]]))
