(ns converge.core-test
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [converge.util  :as util]
            [converge.core :as core]))

(deftest lamport-timestamps
  (let [a (core/random-id-member)
        b (core/random-id-member)
        c (core/random-id-member)]
    (testing "latest-id with absolute max counter"
      (let [ops (core/make-log
                 (core/make-id a 0) :foo
                 (core/make-id b 0) :foo
                 (core/make-id b 1) :foo
                 (core/make-id b 2) :foo
                 (core/make-id c 0) :foo
                 (core/make-id c 1) :foo)]
        (is (= (core/make-id b 2) (core/latest-id ops)))))
    (testing "latest-id with tie for max counter"
      (let [ops (core/make-log
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
               (core/latest-id ops)))))
    (testing "next-id on empty log"
      (let [ops (core/make-log)]
        (is (= (core/make-id a) (core/next-id ops a)))))
    (testing "next-id on non-empty log"
      (let [ops (core/make-log
                 (core/make-id a 0) :foo
                 (core/make-id b 0) :foo
                 (core/make-id b 1) :foo
                 (core/make-id b 2) :foo
                 (core/make-id c 0) :foo
                 (core/make-id c 1) :foo)]
        (is (= (core/make-id c 3) (core/next-id ops c)))))))
