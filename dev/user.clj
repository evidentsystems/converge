(ns ^:no-doc user
  (:require [clojure.repl :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.spec.test.alpha :as stest]
            [eftest.runner :as eftest]))

(set! *warn-on-reflection* true)

(ns-unmap *ns* 'test)

(defn test []
  (eftest/run-tests (eftest/find-tests "test") {:multithread? false}))

(defn instrument
  []
  (stest/instrument))

(defn unstrument
  []
  (stest/unstrument))
