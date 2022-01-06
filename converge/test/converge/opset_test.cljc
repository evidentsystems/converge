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
            [converge.api :as convergent]))

(deftest editscript-addition-ops
  (testing "Add to map"
    (let [original-value {:foo :bar}
          new-value      {:foo :bar :baz :quux}
          r              (convergent/ref original-value :backend :opset)]
      (is (= (reset! r new-value) new-value))))
  (testing "Add to vector"
    (let [original-value [:foo :bar :quux]
          new-value      [:foo :bar :baz :quux]
          r              (convergent/ref original-value :backend :opset)]
      (is (= (reset! r new-value) new-value))))
  (testing "Add to set"
    (let [original-value #{:foo :baz}
          new-value      #{:foo :bar :baz}
          r              (convergent/ref original-value :backend :opset)]
      (is (= (reset! r new-value) new-value))))
  (testing "Add to list"
    (let [original-value '(:foo :bar :quux)
          new-value      '(:foo :bar :baz :quux)
          r              (convergent/ref original-value :backend :opset)]
      (is (= (reset! r new-value) new-value)))))

(deftest editscript-removal-ops
  (testing "Remove from map"
    (let [original-value {:foo :bar :baz :quux}
          new-value      {:foo :bar}
          r              (convergent/ref original-value :backend :opset)]
      (is (= (reset! r new-value) new-value))))
  (testing "Remove from vector"
    (let [original-value [:foo :bar :baz :quux]
          new-value      [:foo :bar :quux]
          r              (convergent/ref original-value :backend :opset)]
      (is (= (reset! r new-value) new-value))))
  (testing "Remove from set"
    (let [original-value #{:foo :bar :baz}
          new-value      #{:foo :baz}
          r              (convergent/ref original-value :backend :opset)]
      (is (= (reset! r new-value) new-value))))
  (testing "Remove from list"
    (let [original-value '(:foo :bar :baz :quux)
          new-value      '(:foo :bar :quux)
          r              (convergent/ref original-value :backend :opset)]
      (is (= (reset! r new-value) new-value)))))

(deftest editscript-replacement-ops
  ;; cases:
  ;;   - replacement with collection of same type (preserve entity id, add ops to patch for diff)
  ;;   - replacement with collection of different type (new entity id, add new ops to patch for diff)
  ;;   - replacement of primitive with collection
  ;;   - replacement of collection with primitive
  (testing "Replacement of root"
    (is true)))

;; cases:
;;  - move primitive/collection to new position in a list
;;  - move collection to new position in a list and edit that collection
;;  - move a subtree to a different position in tree (including pouring existing values from one collection into another)
;;  - rename a map key
(deftest atomic-tree-move
  (testing "Move collection to new position in a tree"
    (let [original-value [{:foo :bar :baz [{:quux "lalala"}]}]
          r              (convergent/ref original-value :backend :opset)
          tree-move      (fn [tree]
                           (let [branch (get-in tree [0 :baz 0])]
                             (-> tree
                                 (update-in [0 :baz] pop)
                                 (conj branch))))]
      (swap! r tree-move)
      (is (= (tree-move original-value) @r))))
  (testing "Move collection to new position in a tree with edits"
    (let [original-value [{:foo :bar :baz [{:quux "lalala"}]}]
          r              (convergent/ref original-value :backend :opset)
          tree-move      (fn [tree]
                           (let [branch (get-in tree [0 :baz 0])]
                             (-> tree
                                 (update-in [0 :baz] pop)
                                 (conj (assoc branch :new :key)))))]
      (swap! r tree-move)
      (is (= (tree-move original-value) @r))))
  (testing "Concurrent movements of a collection to a new tree position"
    (let [original-value [{:foo :bar :baz [{:quux "lalala"}]}]
          actor1         #uuid "4836e7d6-d821-4ce9-bf1d-3fc65dc66847"
          r1             (convergent/ref original-value
                                         :actor actor1
                                         :backend :opset)
          actor2         #uuid "4836e7d6-d821-4ce9-bf1d-3fc65dc66848" ;; gt actor1
          r2             (convergent/ref-from-ops
                          (convergent/ref-log r1)
                          :actor actor2)
          tree-move1     (fn [tree]
                           (let [branch (get-in tree [0 :baz 0])]
                             (-> tree
                                 (update-in [0 :baz] pop)
                                 (conj branch))))
          tree-move2     (fn [tree]
                           (let [branch (get-in tree [0 :baz 0])]
                             (-> tree
                                 (update-in [0 :baz] pop)
                                 (assoc-in [0 :lalala] branch))))]
      (swap! r1 tree-move1)
      (swap! r2 tree-move2)
      (convergent/merge! r1 r2)
      (convergent/merge! r2 r1)
      (is (= @r1 @r2 (tree-move2 original-value)))))
  (testing "Move collection to new position in a tree with edits forcing replace"
    (let [original-value [{:foo :bar :baz [{:quux "lalala"}]}]
          r              (convergent/ref original-value :backend :opset)
          tree-move      (fn [tree]
                           (let [branch (get-in tree [0 :baz 0])]
                             (-> tree
                                 (update-in [0 :baz] pop)
                                 (conj (assoc branch :foo :bar)))))]
      (swap! r tree-move)
      (is (= (tree-move original-value) @r))))
  ;; TODO: https://github.com/evidentsystems/converge/issues/35
  #_(testing "Concurrent movements of a primitive to a new tree position"
      (let [original-value [{:foo  :bar
                             :baz  [:foo]
                             :quux :bar}]
            actor1         #uuid "4836e7d6-d821-4ce9-bf1d-3fc65dc66847"
            r1             (convergent/ref original-value
                                           :actor actor1
                                           :backend :opset)
            actor2         #uuid "4836e7d6-d821-4ce9-bf1d-3fc65dc66848" ;; gt actor1
            r2             (convergent/ref-from-ops
                            (convergent/ref-log r1)
                            :actor actor2)
            tree-move1     (fn [tree]
                             (let [branch (get-in tree [0 :baz 0])]
                               (-> tree
                                   (update-in [0 :baz] pop)
                                   (conj branch))))
            tree-move2     (fn [tree]
                             (let [branch (get-in tree [0 :baz 0])]
                               (-> tree
                                   (update-in [0 :baz] pop)
                                   (assoc-in [0 :lalala] branch))))]
        (swap! r1 tree-move1)
        (swap! r2 tree-move2)
        (convergent/merge! r1 r2)
        (convergent/merge! r2 r1)
        (is (= @r1 @r2 (tree-move2 original-value))))))

(deftest primitive-value-caching
  ;; Ensure that opset growth remains small when many equal primitive values are added
  (testing "Root value caching"
    (let [r (convergent/ref :foo :backend :opset)]
      (is (= @r :foo))
      (reset! r {:bar :foo})
      (is (= @r {:bar :foo})))))

;; Ensure that opset growth remains small when many equal keys are added
(deftest key-caching
  (is true))
