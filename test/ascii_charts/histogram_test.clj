;; Copyright (c) 2017 Paolo Estrella
(ns ascii-charts.histogram-test
  (:require [clojure.test :refer :all]
            [ascii-charts.histogram :refer :all]))

(deftest test-line-coordinates
  (testing "4 coordinates between 0 to 1500"
    (is (= [0 375 750 1125 1500] (line-coords 1500 4))))

  (testing "one coordinate"
    (is (= [0 10] (line-coords 10 1))))

  (testing "zero coordinates"
    (is (= [0] (line-coords 10 0))))

  (testing "lazy coordinates"
    (is (= [0 100 200 300 400]
           (->> (line-coords 1000000 10000)
                (take 5))))))
