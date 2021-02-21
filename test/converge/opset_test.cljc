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
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing run-tests]])
            [converge.util  :as util]
            [converge.opset :as opset]))

(deftest lamport-timestamps
  (let [a (util/uuid)
        b (util/uuid)
        c (util/uuid)]
    (testing "latest-id with absolute max counter"
      (let [opset (opset/opset
                   (opset/make-id a 0) :foo
                   (opset/make-id b 0) :foo
                   (opset/make-id b 1) :foo
                   (opset/make-id b 2) :foo
                   (opset/make-id c 0) :foo
                   (opset/make-id c 1) :foo)]
        (is (= (opset/make-id b 2) (opset/latest-id opset)))))
    (testing "latest-id with tie for max counter"
      (let [opset (opset/opset
                   (opset/make-id a 0) :foo
                   (opset/make-id b 0) :foo
                   (opset/make-id b 1) :foo
                   (opset/make-id b 2) :foo
                   (opset/make-id c 0) :foo
                   (opset/make-id c 1) :foo
                   (opset/make-id c 2) :foo)]
        (is (= (last (sort
                      [(opset/make-id c 2)
                       (opset/make-id b 2)]))
               (opset/latest-id opset)))))
    (testing "next-id on empty opset"
      (let [opset (opset/opset)]
        (is (= opset/root-id (opset/next-id opset a)))))
    (testing "next-id on non-empty opset"
      (let [opset (opset/opset
                   (opset/make-id a 0) :foo
                   (opset/make-id b 0) :foo
                   (opset/make-id b 1) :foo
                   (opset/make-id b 2) :foo
                   (opset/make-id c 0) :foo
                   (opset/make-id c 1) :foo)]
        (is (= (opset/make-id c 3) (opset/next-id opset c)))))))

(comment

  (def a {:foo  [:bar [1 2 3] {:baz [1 2 3 4] :remove :me}]
          :bar  {:a :b :c :d}
          :baz  {}
          :quux [2]})

  (def b {:foo  [:bar :doh {:baz [1 3 5] :la {:foo [1 2 3]}}]
          :bar  [1 2 3]
          :baz  :quux
          :quux [1 2]})

  (def r (converge.api/ref a))
  @r
  (reset! r b)

  (def a [{} [] :key {:nested {:key [1 2 3]}} [:foo "bar" 0 {:nested :inalist}] #{1 4 3 2 5}])
  (def b [{} [] :key {:nested {:key [1 2 3]}} [:foo "bar" {:nested :inalist}] #{1 4 3 2 5}])

  (require '[editscript.core :as editscript]
           '[editscript.edit :as edit])

  (def es
    (some->> b
             (editscript/diff a)
             edit/get-edits))

  (clojure.pprint/pprint es)

  )
