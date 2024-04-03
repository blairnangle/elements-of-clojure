(ns idioms-test
  (:require [clojure.test :refer [deftest is testing]]
            [elements-of-clojure.idioms :as idioms])
  (:import (clojure.lang Cons PersistentHashSet PersistentList PersistentList$EmptyList PersistentVector)))

(def add (fn [x y] (+ x y)))

(defn monoid-add
  ([] 0)
  ([x] x)
  ([x y] (+ x y)))

(deftest should-accumulate-using-concat
  (testing "should throw exception if no `initial` passed to `reducer` along with an empty coll"
    (is (thrown? Exception (idioms/reducer add '()))))

  (testing "should return `initial` if provided along with an empty coll"
    (is
      (and
        (= (idioms/reducer add '() 0) 0)
        (= (idioms/reducer add '() 1) 1))))

  (testing "should return identity value if called on empty coll"
    (is (and
          (= (idioms/reducer monoid-add '()) 0)
          (= (idioms/reducer monoid-add {})) 0)))

  (testing "should behave as expected when called in single-element coll"
    (is (= (idioms/reducer monoid-add '(42))) 42))

  (testing "should still be able accumulate as normal"
    (is (= (idioms/reducer monoid-add '(1 2 3))) 6))

  (testing "should behave as expected for all arities"
    (is (= (idioms/custom-concat) '()))
    (is (= (idioms/custom-concat [" "]) '(" ")))
    (is (= (idioms/custom-concat ["a"]) '("a")))
    (is (= (idioms/custom-concat ["a"] ["b"]) '("a" "b")))
    (is (= (idioms/custom-concat ["a"] ["b"] ["c"]) '("a" "b" "c")))
    (is (= (idioms/custom-concat ["a"] ["b"] ["c"] ["d"]) '("a" "b" "c" "d")))))


(deftest should-accumulate-using-conj
  (testing "should behave as expected for all arities, including returning correct coll type"
    (let [result (idioms/custom-conj)]
      (is
        (and
          (= result [])
          (= (type result) PersistentVector))))

    (let [result (idioms/custom-conj '())]
      (is
        (and
          (= result [])
          (= (type result) PersistentVector))))

    (let [result (idioms/custom-conj '() :a)]
      (is
        (and
          (= result '(:a))
          (= (type result) PersistentList))))

    (let [result (idioms/custom-conj '(:a) :b)]
      (is
        (and
          (= result '(:b :a))
          (= (type result) PersistentList))))

    (let [result (idioms/custom-conj '(:a) :b :c :d)]
      (is
        (and
          (= result '(:d :c :b :a))
          (= (type result) PersistentList))))

    (let [result (idioms/custom-conj [:a] :b)]
      (is
        (and
          (= result [:a :b])
          (= (type result) PersistentVector))))

    (let [result (idioms/custom-conj [:a] :b :c :d)]
      (is
        (and
          (= result [:a :b :c :d])
          (= (type result) PersistentVector))))

    (let [result (idioms/custom-conj #{})]
      (is
        (and
          (= result #{})
          (= (type result) PersistentHashSet))))

    (let [result (idioms/custom-conj #{:a} :b)]
      (is
        (and
          (= result #{:a :b})
          (= (type result) PersistentHashSet))))

    (let [result (idioms/custom-conj #{:a} :b :c :d)]
      (is
        (and
          (= result #{:a :b :c :d})
          (= (type result) PersistentHashSet))))))

(deftest should-accumulate-using-cons
  (testing "should behave as expected for all arities, including returning correct coll type"
    (let [result (idioms/custom-cons)]
      (is
        (and
          (= result [])
          (= (type result) PersistentVector))))

    (let [result (idioms/custom-cons '())]
      (is
        (and
          (= result [])
          (= (type result) PersistentList$EmptyList))))

    (let [result (idioms/custom-cons :a '())]
      (is
        (and
          (= result '(:a))
          (= (type result) Cons))))

    (let [result (idioms/custom-cons :a '(:b))]
      (is
        (and
          (= result '(:a :b))
          (= (type result) Cons))))

    (let [result (idioms/custom-cons :a '(:b :c :d))]
      (is
        (and
          (= result '(:a :b :c :d))
          (= (type result) Cons))))

    (let [result (idioms/custom-cons [])]
      (is
        (and
          (= result [])
          (= (type result) PersistentVector))))

    (let [result (idioms/custom-cons [:a])]
      (is
        (and
          (= result [:a])
          (= (type result) PersistentVector))))

    (let [result (idioms/custom-cons :b [:a])]
      (is
        (and
          (= result [:b :a])
          (= (type result) Cons))))

    (let [result (idioms/custom-cons :c [:a :b])]
      (is
        (and
          (= result [:c :a :b])
          (= (type result) Cons))))))

(deftest should-calculate-pi
  (let [expected 3.14]
    (testing "should calculate pi to n digits (provided n is tiny…)"
      (is (= (idioms/pi-positional 3) expected))
      (is (= (idioms/pi-option-map 3) expected))
      (is (= (idioms/pi-option-map-no-& 3 {}) expected)))

    (testing "should calculate efficiently"
      (is (= (idioms/pi-positional 3 true) expected))
      (is (= (idioms/pi-option-map 3 :efficiently? true) expected))
      (is (= (idioms/pi-option-map-no-& 3 {:efficiently? true}) expected)))

    (testing "should calculate efficiently and correctly"
      (is (= (idioms/pi-positional 3 true true) expected))
      (is (= (idioms/pi-option-map 3 :efficiently? true :correctly true) expected))
      (is (= (idioms/pi-option-map-no-& 3 {:efficiently? true :correctly true}) expected)))

    (testing "should always return the same result if correctly? is false, regardless of n"
      (let [results-positional (map #(idioms/pi-positional % true false) [0 10 100 1000])
            results-option-map (map #(idioms/pi-option-map % :efficiently? true :correctly false) [0 10 100 1000])
            results-option-map-no-& (map #(idioms/pi-option-map-no-& % {:efficiently? true :correctly false}) [0 10 100 1000])]
        (is (apply = results-positional))
        (is (apply = results-option-map))
        (is (apply = results-option-map-no-&))))

    (testing "should take a while if efficiently? is false, but still produce the same result"
      (is (= (idioms/pi-positional 3 false) 3.14))
      (is (= (idioms/pi-option-map 3 :efficiently? false) 3.14))
      (is (= (idioms/pi-option-map-no-& 3 {:efficiently? false}) 3.14)))))
