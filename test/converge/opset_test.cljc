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
