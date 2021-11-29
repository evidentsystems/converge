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
(ns ^:no-doc user
  (:require [clojure.repl :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.spec.test.alpha :as stest]
            [clojure.tools.namespace.repl :refer [refresh]]
            [kaocha.repl :as kaocha]
            [criterium.core :as criterium]
            [clj-async-profiler.core :as profiler]))

(set! *warn-on-reflection* true)

(ns-unmap *ns* 'test)

(defn test []
  (refresh)
  (kaocha/run :unit-clj {:config-file "test/tests.edn"}))

(defn instrument
  []
  (stest/instrument))

(defn unstrument
  []
  (stest/unstrument))

(comment

  (require '[converge.api :as converge]
           '[converge.opset :as opset]
           '[converge.interpret :as interpret]
           '[converge.edn :as edn]
           '[criterium.core :as criterium])

  (def m (converge/ref {}))
  @m

  (def v (converge/ref []))
  @v

  (def a
    {:empty-m {}
     :empty-l []
     :a       :key
     :another {:nested {:key [1 2 3]}}
     :a-list  [:foo "bar" 'baz {:nested :inalist}]
     :a-set   #{1 2 3 4 5}})

  (criterium/bench
   (let [cr (converge/ref a)]
     @cr))

  (simple-benchmark [m a f converge/ref] (f m) 1000)

  (def c (converge/ref a))
  @c
  (converge/opset c)

  (simple-benchmark [c (converge/ref a) v (random-uuid)] (swap! c assoc :foo v) 1000)

  (def b
    {:empty-m {}
     :empty-l []
     :a       :nother
     :another {:nested {:key    [1 2 3]
                        :deeply :mcnested}}
     :a-list  [:foo "bar" {:nested :inalist}]
     :a-set   #{1 3 4 5}})

  (def c (converge/ref {}))
  (reset! c b)
  (reset! c {})

  (swap! c assoc :foo 'bar :baz "quux")
  (swap! c dissoc :foo)

  (binding [*print-meta* true]
    (prn @c))

  (criterium/bench
   (swap! c dissoc :a))
  @c

  (def a {:empty-m {}
          :empty-l []
          :a       :key
          :another {:nested {:key [1 2 3]}}
          :a-list  [:foo "bar" 'baz {:nested :inalist}]
          :a-set   #{1 2 3 4 5}})

  (def o (atom (opset root-id (make-map))))
  (def i (make-id))

  (swap! o into (ops-from-diff @o (:actor i) {} a))
  (converge.edn/edn @o)
  (count @o)
  (editscript/diff {} a)

  (def b {:a       {:different-key :value}
          :another {:nested {:key [2]}}
          :a-list  [:foo :baz {:nested :outalist} :quux]
          :a-set   #{1 2 4 5}
          :b       {:a {:nested {:map :thingy}}}})
  (def b {:baz :quux :a-list [:item2]})

  (count @o)

  (swap! o add-ops-from-diff (:actor i) (converge.edn/edn @o) b)

  (converge.edn/edn @o)

  (editscript/diff a b)

  )
