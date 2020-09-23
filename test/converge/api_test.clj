(ns converge.api-test
  (:require [clojure.test :refer :all]
            [converge.api :as converge]))

(def a {:empty-m {}
        :empty-l []
        :a       :key
        :another {:nested {:key [1 2 3]}}
        :a-list  [:foo "bar" 'baz {:nested :inalist}]
        :a-set   #{1 2 3 4 5}})

(deftest convergent-ref-of-map
  (testing "Can initialize convergent ref with an empty map"
    (is (converge/ref {})))
  (testing "Can initialize convergent ref with a non-empty map"
    (is (converge/ref a)))
  (testing "Can reset to an empty map"
    (let [c (converge/ref a)]
      (is (= {} (reset! c {})))))
  (testing "Can add a top-level key"
    (let [c (converge/ref a)]
      (is (= (assoc a :foo :bar) (swap! c assoc :foo :bar)))))
  (testing "Can remove a top-level key"
    (let [c (converge/ref a)]
      (is (= (assoc a :foo :bar) (swap! c assoc :foo :bar)))))
  (testing "Can add a nested key"
    (let [c (converge/ref a)]
      (is (= (assoc-in a [:foo :bar] :baz)
             (swap! c assoc-in [:foo :bar] :baz)))))
  (testing "Can add a nested vector"
    (let [c (converge/ref a)]
      (is (= (assoc a :foo [:baz])
             (swap! c assoc :foo [:baz])))))
  (testing "Can update a nested vector"
    (let [c (converge/ref a)]
      (is (= (update a :empty-l conj :baz)
             (swap! c update :empty-l conj :baz)))))
  (testing "Can update deeply nested path"
    (let [c (converge/ref a)]
      (is (= (update-in a [:a-list 3] assoc :foo :bar)
             (swap! c update-in [:a-list 3] assoc :foo :bar))))))
