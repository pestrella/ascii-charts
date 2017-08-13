;; Copyright (c) 2017 Paolo Estrella
(ns ascii-charts.histogram-test
  (:require [clojure.test :refer :all]
            [ascii-charts.histogram :refer :all]))

(deftest test-ticks
  (testing "0 to 1500 in 4 ticks"
    (is (= [0 375 750 1125 1500] (ticks 1500 4))))

  (testing "one tick"
    (is (= [0 10] (ticks 10 1))))

  (testing "zero ticks"
    (is (= [0] (ticks 10 0))))

  (testing "lazy ticks"
    (is (= [0 100 200 300 400]
           (->> (ticks 1000000 10000)
                (take 5))))))
