;; Copyright (c) 2017 Paolo Estrella
(ns ascii-charts.histogram-test
  (:require [clojure.test :refer :all]
            [ascii-charts.histogram :refer :all]))

(deftest test-quarters
  (testing "1500 in quartes"
    (is (= [375 750 1125 1500] (quarters 1500)))))
