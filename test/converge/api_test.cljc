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
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing run-tests]])
            [converge.ref :as ref]
            [converge.api :as convergent]))

(def a {:empty-m {}
        :empty-l []
        :a       :key
        :another {:nested {:key [1 2 3]}}
        :a-list  [:foo "bar" 'baz {:nested :inalist}]
        :a-set   #{1 2 3 4 5}})

(deftest convergent-ref-of-map
  (testing "Can initialize convergent ref with an empty map"
    (is (convergent/ref {})))
  (testing "Can initialize convergent ref with a non-empty map"
    (is (convergent/ref a)))
  (testing "Can reset to an empty map"
    (let [c (convergent/ref a)]
      (is (= {} (reset! c {})))))
  (testing "Can add a top-level key"
    (let [c (convergent/ref a)]
      (is (= (assoc a :foo :bar) (swap! c assoc :foo :bar)))))
  (testing "Can remove a top-level key"
    (let [c (convergent/ref a)]
      (is (= (assoc a :foo :bar) (swap! c assoc :foo :bar)))))
  (testing "Can add a nested key"
    (let [c (convergent/ref a)]
      (is (= (assoc-in a [:foo :bar] :baz)
             (swap! c assoc-in [:foo :bar] :baz)))))
  (testing "Can add a nested vector"
    (let [c (convergent/ref a)]
      (is (= (assoc a :foo [:baz])
             (swap! c assoc :foo [:baz])))))
  (testing "Can update a nested vector"
    (let [c (convergent/ref a)]
      (is (= (update a :empty-l conj :baz)
             (swap! c update :empty-l conj :baz)))))
  (testing "Can update deeply nested path"
    (let [c (convergent/ref a)]
      (is (= (update-in a [:a-list 3] assoc :foo :bar)
             (swap! c update-in [:a-list 3] assoc :foo :bar))))))

(deftest merging
  (let [c (convergent/ref a)
        d (convergent/ref-from-opset (convergent/opset c))]
      (swap! d assoc :b :another-key)
      (testing "merging another convergent ref"
        (is (= @d @(convergent/merge! (convergent/ref-from-opset (convergent/opset c))
                                      d))))
      (testing "merging a patch"
        (is (= @d @(convergent/merge! (convergent/ref-from-opset (convergent/opset c))
                                      (convergent/peek-patches d)))))))

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
        (is (= final @(convergent/squash! cr d)))
        (is (> (count (convergent/opset d))
               (count (convergent/opset cr))))))
    (testing "squashing a patch"
      (let [cr (convergent/ref-from-opset (convergent/opset c))]
        (is (= final @(convergent/squash! cr (ref/->Patch (merge (:ops patch1) (:ops patch2))))))
        (is (> (count (convergent/opset d))
               (count (convergent/opset cr))))))))
