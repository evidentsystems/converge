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
            [converge.core :as core]))

(deftest lamport-timestamps
  (let [a (util/uuid)
        b (util/uuid)
        c (util/uuid)]
    (testing "latest-id with absolute max counter"
      (let [opset (core/opset
                   (core/make-id a 0) :foo
                   (core/make-id b 0) :foo
                   (core/make-id b 1) :foo
                   (core/make-id b 2) :foo
                   (core/make-id c 0) :foo
                   (core/make-id c 1) :foo)]
        (is (= (core/make-id b 2) (core/latest-id opset)))))
    (testing "latest-id with tie for max counter"
      (let [opset (core/opset
                   (core/make-id a 0) :foo
                   (core/make-id b 0) :foo
                   (core/make-id b 1) :foo
                   (core/make-id b 2) :foo
                   (core/make-id c 0) :foo
                   (core/make-id c 1) :foo
                   (core/make-id c 2) :foo)]
        (is (= (last (sort
                      [(core/make-id c 2)
                       (core/make-id b 2)]))
               (core/latest-id opset)))))
    (testing "next-id on empty opset"
      (let [opset (core/opset)]
        (is (= core/root-id (core/next-id opset a)))))
    (testing "next-id on non-empty opset"
      (let [opset (core/opset
                   (core/make-id a 0) :foo
                   (core/make-id b 0) :foo
                   (core/make-id b 1) :foo
                   (core/make-id b 2) :foo
                   (core/make-id c 0) :foo
                   (core/make-id c 1) :foo)]
        (is (= (core/make-id c 3) (core/next-id opset c)))))))
