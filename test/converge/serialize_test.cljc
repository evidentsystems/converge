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
(ns converge.serialize-test
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            #?(:clj  [clojure.test.check.clojure-test :refer [defspec]]
               :cljs [clojure.test.check.clojure-test :refer-macros [defspec]])
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop #?@(:cljs [:include-macros true])]
            [converge.api :as convergent]
            [converge.serialize :as serialize]
            [cognitect.transit :as t])
  #?(:clj (:import [java.io ByteArrayInputStream ByteArrayOutputStream])))

(defn tagged-map-value
  [rec]
  (t/tagged-value "map" rec))

(def read-handlers*
  {"converge/id"             (t/read-handler serialize/read-id)
   "converge/op"             (t/read-handler serialize/read-operation)
   "converge/patch"          (t/read-handler serialize/read-patch)
   "converge/element"        (t/read-handler serialize/read-element)
   "converge/interpretation" (t/read-handler serialize/read-interpretation)
   "converge/state"          (t/read-handler serialize/read-state)
   "converge/ref"            (t/read-handler serialize/read-ref)
   "avl/map"                 (t/read-handler serialize/read-avl-map)})

(def read-handlers
  (merge #?(:clj
            t/default-read-handlers

            :cljs
            {"u" (t/read-handler uuid)})
         read-handlers*))

(def #?(:clj  write-handlers*
        :cljs write-handlers)
  {converge.opset.Id                 (t/write-handler (constantly "converge/id") tagged-map-value)
   converge.opset.Op                 (t/write-handler (constantly "converge/op") tagged-map-value)
   converge.patch.Patch              (t/write-handler (constantly "converge/patch") tagged-map-value)
   converge.interpret.Element        (t/write-handler (constantly "converge/element") tagged-map-value)
   converge.interpret.Interpretation (t/write-handler (constantly "converge/interpretation") tagged-map-value)
   converge.ref.ConvergentState      (t/write-handler (constantly "converge/state") serialize/write-state)
   converge.ref.ConvergentRef        (t/write-handler (constantly "converge/ref") serialize/write-ref)
   clojure.data.avl.AVLMap           (t/write-handler (constantly "avl/map") serialize/write-avl-map)})

#?(:clj
   (def write-handlers
     (merge t/default-write-handlers write-handlers*)))

#?(:cljs
   (do
     (def writer
       (t/writer :json {:handlers write-handlers}))

     (defn write-str
       [o]
       (t/write writer o))

     (def reader
       (t/reader :json {:handlers read-handlers}))

     (defn read-str
       [s]
       (t/read reader s)))

   :clj
   (do
     (defn writer
       [out]
       (t/writer out :json {:handlers write-handlers}))

     (defn write-str
       [o]
       (let [out (ByteArrayOutputStream. 4096)]
         (t/write (writer out) o)
         (str out)))

     (defn reader
       [in]
       (t/reader in :json {:handlers read-handlers}))

     (defn read-str
       [s]
       (let [in (ByteArrayInputStream. (.getBytes s "utf-8"))]
         (t/read (reader in))))))

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

(deftest transit-roundtrip-example
  (testing "map"
    (let [ref (convergent/ref a)
          rt  (read-str (write-str ref))]
      (is (= a @ref @rt))))
  (testing "list"
    (let [ref (convergent/ref b)
          rt  (read-str (write-str ref))]
      (is (= b @ref @rt)))))

#_(defspec transit-roundtrip 100
  (prop/for-all
   [v (gen/one-of [(gen/vector gen/any) (gen/map gen/any gen/any)])]
   (let [ref (convergent/ref v)
         rt  (read-str (write-str ref))]
     (= v @ref @rt))))

(comment ;; Clojure benchmarks

  (require '[criterium.core :as criterium])

  (def r (convergent/ref a))

  (criterium/bench (read-str (write-str r)))

  ;; Macbook Pro 02/17/2021
  ;;                 Evaluation count : 135480 in 60 samples of 2258 calls.
  ;;              Execution time mean : 447.208401 µs
  ;;     Execution time std-deviation : 7.824025 µs
  ;;    Execution time lower quantile : 439.760365 µs ( 2.5%)
  ;;    Execution time upper quantile : 469.554066 µs (97.5%)
  ;;                    Overhead used : 7.672645 ns

  ;; Found 4 outliers in 60 samples (6.6667 %)
  ;; 	low-severe	 2 (3.3333 %)
  ;; 	low-mild	 2 (3.3333 %)
  ;;  Variance from outliers : 6.2885 % Variance is slightly inflated by outliers


  )

(comment ;; ClojureScript benchmarks

  (simple-benchmark
   [r (convergent/ref a)]
   (read-str (write-str r))
   10000)

  ;; Macbook Pro, Chrome 02/17/2021
  ;; [r (convergent/ref a)], (read-str (write-str r)), 10000 runs, 7310 msecs

  )
