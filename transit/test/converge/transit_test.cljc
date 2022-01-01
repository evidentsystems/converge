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
(ns converge.transit-test
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            #?(:clj  [clojure.test.check.clojure-test :refer [defspec]]
               :cljs [clojure.test.check.clojure-test :refer-macros [defspec]])
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop #?@(:cljs [:include-macros true])]
            [converge.api :as convergent]
            [converge.transit :as converge-transit]
            [cognitect.transit :as t])
  #?(:clj (:import [java.io ByteArrayInputStream ByteArrayOutputStream])))

#?(:cljs
   (do
     (def writer
       (t/writer :json {:handlers converge-transit/write-handlers}))

     (defn write-str
       [o]
       (t/write writer o))

     (def reader
       (t/reader :json {:handlers converge-transit/read-handlers}))

     (defn read-str
       [s]
       (t/read reader s)))

   :clj
   (do
     (defn writer
       [out]
       (t/writer out :json {:handlers converge-transit/write-handlers}))

     (defn write-str
       [o]
       (let [out (ByteArrayOutputStream. 4096)]
         (t/write (writer out) o)
         (str out)))

     (defn reader
       [in]
       (t/reader in :json {:handlers converge-transit/read-handlers}))

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

(defspec transit-roundtrip 100
  (prop/for-all
   [v (gen/one-of [(gen/vector gen/any-equatable)
                   (gen/map gen/any-equatable gen/any-equatable)
                   (gen/set gen/any-equatable)
                   (gen/list gen/any-equatable)
                   (gen/container-type gen/any-equatable)])]
   (let [ref (convergent/ref v)
         rt  (read-str (write-str ref))]
     (= v @ref @rt))))

;; clj
(comment
  ;; Benchmark
  (require '[criterium.core :as criterium])

  (def r (convergent/ref a))

  (criterium/bench (read-str (write-str r)))

  (require '[clojure.edn :as edn]
           '[clojure.java.io :as io])

  (def value
    (with-open [pbr (java.io.PushbackReader. (io/reader (io/file "big-tree.edn")))]
      (edn/read pbr)))

  (with-open [os (io/output-stream (io/file "big-tree.transit.json"))]
    (t/write (t/writer os :json {:handlers converge-transit/write-handlers})
             value))

  (with-open [os (io/output-stream (io/file "big-tree.transit.msgpack"))]
    (t/write (t/writer os :msgpack {:handlers converge-transit/write-handlers})
             value))

  (def r (convergent/ref value))

  (with-open [os (io/output-stream (io/file "big-tree-convergent-ref.transit.json"))]
    (t/write (t/writer os :json {:handlers converge-transit/write-handlers})
             r))

  (with-open [os (io/output-stream (io/file "big-tree-convergent-ref.transit.msgpack"))]
    (t/write (t/writer os :msgpack {:handlers converge-transit/write-handlers})
             r))

  (.length (io/file "big-tree.edn"))

  (.length (io/file "big-tree.transit.json"))
  (.length (io/file "big-tree-convergent-ref.transit.json"))

  (quot (.length (io/file "big-tree-convergent-ref.transit.json"))
        (.length (io/file "big-tree.transit.json")))

  (.length (io/file "big-tree.transit.msgpack"))
  (.length (io/file "big-tree-convergent-ref.transit.msgpack"))

  (quot (.length (io/file "big-tree-convergent-ref.transit.msgpack"))
        (.length (io/file "big-tree.transit.msgpack")))

  ;; README usage

  (require '[cognitect.transit :as transit]
           '[converge.transit :as converge-transit]
           '[converge.api :as convergent])
  (import [java.io ByteArrayInputStream ByteArrayOutputStream])

  ;; Write data to a stream
  (def out (ByteArrayOutputStream. 4096))
  (def writer
    (transit/writer out :json
                    {:handlers (merge transit/default-write-handlers
                                      converge-transit/write-handlers)}))
  (def cref (convergent/ref {:foo :bar
                             :a   [1 2]}))
  (transit/write writer cref)

  cref

  ;; Take a peek at the JSON
  (.toString out)
  ;; => "[\"~#opset/ref\", <snip a bunch of operations ...>]"

  ;; Read data from a stream
  (def in (ByteArrayInputStream. (.toByteArray out)))
  (def reader
    (transit/reader in :json
                    {:handlers (merge transit/default-read-handlers
                                      converge-transit/read-handlers)}))
  (def read-ref (transit/read reader))
  (prn read-ref)
  ;; => #converge.opset.ref.OpsetConvergentRef[{:status :ready, :val {:a [1 2], :foo :bar}} 0x3bd98fc8]
  (prn @read-ref)
  ;; => {:a [1 2], :foo :bar}

  ;; end README usage
  )

;; cljs
(comment
  ;; Benchmark
  (simple-benchmark
   [r (convergent/ref a)]
   (read-str (write-str r))
   10000)

  ;; README usage
  (ns example
    (:require [cognitect.transit :as t]
              [converge.transit :as converge-transit]
              [converge.api :as convergent]))

  (defn roundtrip [x]
    (let [w (t/writer :json {:handlers converge-transit/write-handlers})
          r (t/reader :json {:handlers converge-transit/read-handlers})]
      (t/read r (t/write w x))))

  (defn test-roundtrip []
    (let [list1 [:red :green :blue]
          list2 [:apple :pear :grape]
          cref  (convergent/ref
                 {(t/integer 1) list1
                  (t/integer 2) list2})
          cref' (roundtrip cref)]
      (assert (= @cref @cref'))))
  ;; end README usage
  )
