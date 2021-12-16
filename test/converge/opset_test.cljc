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
(ns converge.opset-test
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [converge.util  :as util]
            [converge.api :as convergent]
            [converge.opset.edn :as edn]
            [converge.opset.interpret :as interpret]
            [converge.opset.patch :as patch]))

(deftest editscript-addition-ops)

(deftest editscript-removal-ops
  (testing "Remove from map"
    (let [r         (convergent/ref {:foo :bar :baz :quux} :backend :opset)
          new-value {:foo :bar}]
      (is (= (reset! r new-value) new-value))))
  (testing "Remove from vector"
    (let [r         (convergent/ref [:foo :bar :baz :quux] :backend :opset)
          new-value [:foo :bar :quux]]
      (is (= (reset! r new-value) new-value))))
  (testing "Remove from set"
    (let [r (convergent/ref #{:foo :bar :baz} :backend :opset)
          new-value #{:foo :baz}]
      (is (= (reset! r new-value) new-value))))
  (testing "Remove from list"
    (let [r         (convergent/ref '(:foo :bar :baz :quux) :backend :opset)
          new-value '(:foo :bar :quux)]
      (is (= (reset! r new-value) new-value)))))

(deftest editscript-replacement-ops
  ;; cases:
  ;;   - replacement with collection of same type (preserve entity id, add ops to patch for diff)
  ;;   - replacement with collection of different type (new entity id, add new ops to patch for diff)
  ;;   - replacement of primitive with collection
  ;;   - replacement of collection with primitive
  (testing "Replacement of root"))

;; cases:
;;  - move item to new position in a list
;;  - move a subtree to a different position in tree (including pouring existing values from one collection into another)
;;  - rename a map key
(deftest atomic-tree-move
  )

;; Ensure that opset growth remains small when many equal primitive values are added
(deftest primitive-value-caching)

;; Ensure that opset growth remains small when many equal keys are added
(deftest key-caching)
