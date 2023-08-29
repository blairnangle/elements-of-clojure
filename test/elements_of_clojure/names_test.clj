(ns elements-of-clojure.names-test
  (:require [clojure.test :refer [deftest is testing]]
            [elements-of-clojure.names :as names]))

(def galaxy {:sol {:jupiter {:callisto "I am Callisto"
                             :europa   "I am Europa"}
                   :saturn  {:titan  "I am Titan"
                             :phoebe "I am Phoebe"}}})

(deftest should-return-moon-for-galaxy
  (doseq [moon-lookup-fn [names/get-sol-jupiter names/get-jovian-moon]]

    (testing "should return Jovian moon for galaxy"
      (is (= "I am Callisto" (moon-lookup-fn galaxy :callisto))))

    (testing "should return `not-found`"
      (let [not-found "Callisto was not found!"]
        (is (= not-found (names/get-sol-jupiter galaxy :cyllene not-found)))))

    (testing "should return `nil`"
      (is (= nil (names/get-sol-jupiter galaxy :cyllene))))))
