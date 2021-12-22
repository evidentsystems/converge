(ns converge.domain-test
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [converge.domain :as domain]))

(deftest lamport-timestamps
  (let [a (domain/uuid)
        b (domain/uuid)
        c (domain/uuid)]
    (testing "latest-id with absolute max counter"
      (let [ops (domain/make-log
                 (domain/make-id a 0) :foo
                 (domain/make-id b 0) :foo
                 (domain/make-id b 1) :foo
                 (domain/make-id b 2) :foo
                 (domain/make-id c 0) :foo
                 (domain/make-id c 1) :foo)]
        (is (= (domain/make-id b 2) (domain/latest-id ops)))))
    (testing "latest-id with tie for max counter"
      (let [ops (domain/make-log
                 (domain/make-id a 0) :foo
                 (domain/make-id b 0) :foo
                 (domain/make-id b 1) :foo
                 (domain/make-id b 2) :foo
                 (domain/make-id c 0) :foo
                 (domain/make-id c 1) :foo
                 (domain/make-id c 2) :foo)]
        (is (= (last (sort
                      [(domain/make-id c 2)
                       (domain/make-id b 2)]))
               (domain/latest-id ops)))))
    (testing "next-id on empty log"
      (let [ops (domain/make-log)]
        (is (= (domain/make-id a) (domain/next-id ops a)))))
    (testing "next-id on non-empty log"
      (let [ops (domain/make-log
                 (domain/make-id a 0) :foo
                 (domain/make-id b 0) :foo
                 (domain/make-id b 1) :foo
                 (domain/make-id b 2) :foo
                 (domain/make-id c 0) :foo
                 (domain/make-id c 1) :foo)]
        (is (= (domain/make-id c 3) (domain/next-id ops c)))))))
