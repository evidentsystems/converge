;; Copyright 2020 Evident Systems LLC

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
(ns converge.api-test
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            #?(:clj  [clojure.test.check.clojure-test :refer [defspec]]
               :cljs [clojure.test.check.clojure-test :refer-macros [defspec]])
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop #?@(:cljs [:include-macros true])]
            [converge.api :as convergent]
            [converge.patch :as patch]))

(def a {:empty-m {}
        :empty-l []
        :a       :key
        :another {:nested {:key [1 2 3]}}
        :a-list  [:foo "bar" 'baz {:nested :inalist}]
        :a-set   #{1 2 3 4 5}})

(def b [{}
        []
        :key
        {:nested {:key [1 2 3]}}
        [:foo "bar" 'baz {:nested :inalist}]
        #{1 2 3 4 5}])

(deftest all-editscript-cases
  (let [a {:foo  [:bar [1 2 3] {:baz [1 2 3 4] :remove :me}]
           :bar  {:a :b :c :d}
           :baz  {}
           :quux [2]}
        b {:foo  [:bar :doh {:baz [1 3 5] :la {:foo [1 2 3]}}]
           :bar  [1 2 3]
           :baz  :quux
           :quux [1 2]}
        r (convergent/ref a)]
    (reset! r b)
    (is (= @r b))))

(deftest convergent-ref-of-map
  (testing "Can initialize convergent ref with an empty map"
    (is (convergent/ref {})))
  (testing "Can initialize convergent ref with a non-empty map"
    (is (convergent/ref a)))
  (testing "Can reset to an empty map"
    (let [c (convergent/ref a)]
      (is (= (reset! c {}) {}))))
  (testing "Can add a top-level key"
    (let [c (convergent/ref a)]
      (is (= (swap! c assoc :foo :bar) (assoc a :foo :bar)))))
  (testing "Can remove a top-level key"
    (let [c (convergent/ref a)]
      (is (= (swap! c assoc :foo :bar) (assoc a :foo :bar)))))
  (testing "Can add a nested key"
    (let [c (convergent/ref a)]
      (is (= (swap! c assoc-in [:foo :bar] :baz)
             (assoc-in a [:foo :bar] :baz)))))
  (testing "Can add a nested vector"
    (let [c (convergent/ref a)]
      (is (= (swap! c assoc :foo [:baz])
             (assoc a :foo [:baz])))))
  (testing "Can update a nested vector"
    (let [c (convergent/ref a)]
      (is (= (swap! c update :empty-l conj :baz)
             (update a :empty-l conj :baz)))))
  (testing "Can update deeply nested path"
    (let [c (convergent/ref a)]
      (is (= (swap! c update-in [:a-list 3] assoc :foo :bar)
             (update-in a [:a-list 3] assoc :foo :bar)))))
  (testing "Can remove a list element"
    (let [c (convergent/ref a)]
      (is (= (swap! c assoc :a-list [:foo "bar" {:nested :inalist}])
             (assoc a :a-list [:foo "bar" {:nested :inalist}]))))))

(deftest convergent-ref-of-vector
  (testing "Can initialize convergent ref with an empty vector"
    (is (convergent/ref [])))
  (testing "Can initialize convergent ref with a non-empty vector"
    (is (convergent/ref b)))
  (testing "Can reset to an empty vector"
    (let [c (convergent/ref b)]
      (is (= [] (reset! c [])))))
  (testing "Can conj a top-level element"
    (let [c (convergent/ref b)]
      (is (= (conj b :bar) (swap! c conj :bar)))))
  (testing "Can remove a top-level element"
    (let [c (convergent/ref b)]
      (is (= (pop b) (swap! c pop)))))
  (testing "Can add a nested key"
    (let [c (convergent/ref b)]
      (is (= (assoc-in b [0 :foo] :baz)
             (swap! c assoc-in [0 :foo] :baz)))))
  (testing "Can add a nested vector"
    (let [c (convergent/ref b)]
      (is (= (assoc-in b [0 :foo] [:baz])
             (swap! c assoc-in [0 :foo] [:baz])))))
  (testing "Can update a nested vector"
    (let [c (convergent/ref b)]
      (is (= (update b 1 conj :baz)
             (swap! c update 1 conj :baz)))))
  (testing "Can update deeply nested path"
    (let [c (convergent/ref b)]
      (is (= (update-in b [4 3] assoc :foo :bar)
             (swap! c update-in [4 3] assoc :foo :bar)))))
  (testing "Can remove a list element"
    (let [c (convergent/ref b)]
      (is (= (swap! c assoc 4 [:foo "bar" {:nested :inalist}])
             (assoc b 4 [:foo "bar" {:nested :inalist}]))))))

(deftest merging
  (let [c (convergent/ref a)
        d (convergent/ref-from-opset (convergent/opset c))]
    (swap! d assoc :b :another-key)
    (testing "merging nil"
      (is (= a @(convergent/merge! (convergent/ref-from-opset (convergent/opset c))
                                   nil))))
    (testing "merging another convergent ref"
      (is (= @d @(convergent/merge! (convergent/ref-from-opset (convergent/opset c))
                                    d))))
    (testing "merging a patch"
      (is (= @d @(convergent/merge! (convergent/ref-from-opset (convergent/opset c))
                                    (convergent/peek-patches d)))))
    ;; TODO: merging a snapshot ref, with subsequent operations
    ))

(deftest snapshots
  (let [c (convergent/ref a)]
    (testing "snapshotting another convergent ref"
      (let [cr (convergent/snapshot-ref c)]
        (is (= @c @cr))
        (is (= 1 (count (convergent/opset cr))))))
    (testing "adding some values to a snapshot ref"
      (let [cr (convergent/snapshot-ref c)]
        (swap! cr
               assoc
               :b :another-key
               :a :foo)
        (is (= (assoc @c
                      :b :another-key
                      :a :foo)
               @cr))
        (is (< 1 (count (convergent/opset cr))))))))

(deftest squashing
  (let [c      (convergent/ref a)
        d      (convergent/ref-from-opset (convergent/opset c))
        _      (swap! d assoc
                      :b :another-key
                      :a :foo)
        patch1 (convergent/pop-patches! d)
        final  (swap! d dissoc :a)
        patch2 (convergent/pop-patches! d)]
    (testing "squashing another convergent ref"
      (let [cr (convergent/ref-from-opset (convergent/opset c))]
        (is (= @(convergent/squash! cr d) final))
        (is (> (count (convergent/opset d))
               (count (convergent/opset cr))))))
    (testing "squashing a patch"
      (let [cr (convergent/ref-from-opset (convergent/opset c))]
        (is (= final @(convergent/squash! cr (patch/->Patch (merge (:ops patch1) (:ops patch2))))))
        (is (> (count (convergent/opset d))
               (count (convergent/opset cr))))))
    (testing "squashing a snapshot ref"
      (let [initial-count (count (convergent/opset c))
            cr            (convergent/snapshot-ref c)]
        (swap! cr assoc
               :b :another-key
               :a :foo)
        (swap! cr dissoc :a)
        (is (= @(convergent/squash! c cr) final))
        (is (> (count (convergent/opset c))
               initial-count))))))

#_(defspec generated-map 100
  (prop/for-all
   [a (gen/map gen/any gen/any)
    b (gen/map gen/any gen/any)]
   (let [ref (convergent/ref a)]
     (reset! ref b)
     (= @ref b))))

#_(defspec generated-vector 100
  (prop/for-all
   [a (gen/vector gen/any)
    b (gen/vector gen/any)]
   (let [ref (convergent/ref a)]
     (reset! ref b)
     (= @ref b))))

(comment ;; Clojure benchmarks

  (require '[criterium.core :as criterium])

  (criterium/bench
   @(convergent/ref a))

  ;; MacBook Pro 02/17/2021
  ;;                 Evaluation count : 84780 in 60 samples of 1413 calls.
  ;;              Execution time mean : 716.656014 µs
  ;;     Execution time std-deviation : 21.399499 µs
  ;;    Execution time lower quantile : 702.140090 µs ( 2.5%)
  ;;    Execution time upper quantile : 767.655602 µs (97.5%)
  ;;                    Overhead used : 7.672645 ns

  ;; Found 6 outliers in 60 samples (10.0000 %)
  ;; 	low-severe	 4 (6.6667 %)
  ;; 	low-mild	 2 (3.3333 %)
  ;;  Variance from outliers : 17.3517 % Variance is moderately inflated by outliers

  (def r (convergent/ref {}))
  @r

  (criterium/bench
   (do
     (swap! r assoc-in [:foo :bar :baz] :quux)
     (swap! r update dissoc :foo)))

  ;; MacBook Pro 02/22/2021
  ;;                 Evaluation count : 7589040 in 60 samples of 126484 calls.
  ;;              Execution time mean : 8.235078 µs
  ;;     Execution time std-deviation : 340.978588 ns
  ;;    Execution time lower quantile : 7.893320 µs ( 2.5%)
  ;;    Execution time upper quantile : 8.986638 µs (97.5%)
  ;;                    Overhead used : 8.008514 ns

  ;; Found 2 outliers in 60 samples (3.3333 %)
  ;; 	low-severe	 1 (1.6667 %)
  ;; 	low-mild	 1 (1.6667 %)
  ;;  Variance from outliers : 27.1139 % Variance is moderately inflated by outliers


  (def r (convergent/ref a))
  @r

  (criterium/bench
   (do
     (swap! r assoc-in [:foo :bar :baz] :quux)
     (swap! r update dissoc :foo)))

  ;; MacBook Pro 03/25/2021 before patch optimization
  ;;                 Evaluation count : 31560 in 60 samples of 526 calls.
  ;;              Execution time mean : 1.869336 ms
  ;;     Execution time std-deviation : 156.857244 µs
  ;;    Execution time lower quantile : 1.621887 ms ( 2.5%)
  ;;    Execution time upper quantile : 2.221263 ms (97.5%)
  ;;                    Overhead used : 8.250623 ns

  ;; Found 2 outliers in 60 samples (3.3333 %)
  ;; 	low-severe	 2 (3.3333 %)
  ;;  Variance from outliers : 61.8498 % Variance is severely inflated by outliers


  ;; MacBook Pro 02/22/2021 after patch optimization
  ;;                 Evaluation count : 1902960 in 60 samples of 31716 calls.
  ;;              Execution time mean : 33.085470 µs
  ;;     Execution time std-deviation : 1.368126 µs
  ;;    Execution time lower quantile : 30.987825 µs ( 2.5%)
  ;;    Execution time upper quantile : 36.353521 µs (97.5%)
  ;;                    Overhead used : 8.250623 ns

  ;; Found 1 outliers in 60 samples (1.6667 %)
  ;; 	low-severe	 1 (1.6667 %)
  ;;  Variance from outliers : 27.1107 % Variance is moderately inflated by outliers


  )

(comment ;; ClojureScript benchmarks

  (simple-benchmark
   []
   @(convergent/ref a)
   10000)

  ;; Macbook Pro, Chrome 02/17/2021
  ;; [], (let [r (convergent/ref a)] (clojure.core/deref r)), 10000 runs, 15224 msecs

  (simple-benchmark
   [r (convergent/ref {})]
   (do
     (swap! r assoc-in [:foo :bar :baz] :quux)
     (swap! r update dissoc :foo))
   10000)

  ;; Macbook Pro, Chrome 03/25/2021
  ;; [r (convergent/ref {})], (do (swap! r assoc-in [:foo :bar :baz] :quux) (swap! r update dissoc :foo)), 10000 runs, 2022 msecs

  (simple-benchmark
   [r (convergent/ref a)]
   (do
     (swap! r assoc-in [:foo :bar :baz] :quux)
     (swap! r update dissoc :foo))
   1000)

  )
