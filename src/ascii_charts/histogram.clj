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
  (let [max-x (first (max* data))
        max-y (second (max* data))
        x-label-offset (inc (char-len max-x))
        y-label-offset (inc (char-len max-y))
        scale (/ *histo-height* max-y)]
    (->> data
         (map (fn [[_ y]] (fixed-len (char-line "*" (* y scale)) *histo-height*)))
         (apply map str)
         (map double-space))))

(defn print-histo [data]
  (let [max-y (second (max* data))
        y-label-offset (inc (char-len max-y))
        y-labels (into {} (map vector (quarters *histo-height*) (quarters max-y)))]
    (for [row (map-indexed
             (fn [i s]
               (if-let [label (y-labels (- *histo-height* i))]
                 (str (fixed-len (str label) y-label-offset) s)
                 (str (fixed-len " " y-label-offset) s)))
             (histo data))]
      (println row))))
