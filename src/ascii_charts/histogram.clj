;; Copyright (c) 2017 Paolo Estrella
(ns ascii-charts.histogram
  (:import java.lang.Math))

;; TODO: some of these functions can be otimised by memoization

(defn max* [data]
  (apply map max data))

(defn min* [data]
  (apply map min data))

(defn max-len [coll]
  (->> (map str coll)
       (map count)
       (apply max)))

(defn double-space [s]
  (apply str (map #(str " " %) s)))

(defn mid [x y]
  (let [[mn mx] (if (< x y) [x y] [y x])]
    (+ mn (/ (- mx mn) 2))))

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

(defn line-coords
  "Return a sequence of n evenly distributed coordinates on a
  number line between min and max."
  ([mx n]
   (line-coords 0 mx n))
  ([mn mx n]
   (when (>= n 0)
     (let [tick-length (/ (- mx mn) (if (= 0 n) 1 n))
           tick (+ mn tick-length)]
       (lazy-seq
        (cons (int mn) (line-coords tick mx (dec n))))))))

(def ^:dynamic *histo-height* 20)

(defn round-up
  "Returns the number, rounded up by the specified scale"
  [n scale]
  (* (Math/ceil (/ n scale)) scale))

(defn data-plot [data]
  (let [max-y (round-up (second (max* data)) 10)
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

(defn histo
  "Returns a histogram with bins of uniform width, using the number of bins
  if specified."
  [data & [nbins]]
  (let [max-x (round-up (first (max* data)) 10)
        min-x (first (min* data))
        x-coords (line-coords min-x max-x (or nbins 10))
        range-keys (map vector x-coords (rest x-coords))
        spread (->> (map #(vector (first %) 0) range-keys)
                    (into {}))]
    (->> (reduce (fn [data* [x y]]
                  (let [range (some (fn [[lo hi]]
                                      (when (and (>= x lo)
                                                 (< x hi))
                                        [lo hi]))
                                    range-keys)
                        sum (or (data* range) 0)]
                    (assoc data* (first range) (+ sum y))))
                spread
                data)
         (into [])
         (sort))))

(defn draw [data]
  (let [y-vals (map second data)
        max-y (round-up (second (max* data)) 10)
        y-label-offset (inc (max-len y-vals))
        y-labels (into {} (map vector
                               (line-coords *histo-height* 4)
                               (line-coords max-y 4)))
        h (data-plot data)
        chart (map-indexed
               (fn [i s]
                 (if-let [label (y-labels (- *histo-height* i))]
                   (str (fixed-len (str label) y-label-offset) s)
                   (str (fixed-len " " y-label-offset) s)))
               (->> h
                    (apply map str)
                    (map double-space)))]

    (doseq [row (-> (into [] chart)
                  (into (x-axis data)))]
      (println row))))

(comment
  (line-coords 125 10)
  (draw (histo [[0 123]
                [25 32]
                [50 85]
                [75 52]
                [100 102]
                [125 44]]
               6)))
