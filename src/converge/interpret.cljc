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
(ns converge.interpret
  "Functions for interpreting an OpSet as per sections 3.2 and 5.2 of
  the [OpSets paper](https://arxiv.org/pdf/1805.04263.pdf)"
  (:require [clojure.set :as set]
            [converge.opset :as opset])
  #?(:clj (:import [converge.opset Id])))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(defn transitive-closure
  [e]
  (let [e2 (into #{}
                 (for [[a b] e
                       [c d] e
                       :when (= b c)]
                   [a d]))]
    (if (set/subset? e2 e)
      e
      (recur (set/union e e2)))))


(defrecord Element [#?@(:cljs [^clj entity]
                        :clj  [^Id entity])
                    attribute
                    #?@(:cljs [^clj value]
                        :clj  [^Id value])])

(defrecord Interpretation [elements list-links])

(def list-end-sigil ::list-end)

(defn ancestor
  [elements]
  (transitive-closure
   (into #{} (map (juxt :entity :value)) elements)))

(defmulti -interpret-op
  (fn [_agg _id {action :action :as op}] action)
  :default ::default)

(defmethod -interpret-op ::default
  [agg _id _op]
  agg)

(defmethod -interpret-op :snapshot
  [agg id {{{:keys [elements list-links]} :interpretation} :data :as op}]
  (assoc agg :elements elements :list-links list-links))

(defmethod -interpret-op :assign
  [{:keys [elements] :as agg} id {:keys [data] :as op}]
  (let [{:keys [entity attribute value]} data]
    ;; Skip operations that would introduce a cycle from decendent
    ;; (value) to ancestor (entity), per Section 5.2
    (if (contains? (ancestor elements) [value entity])
      agg
      (assoc agg
             :elements
             (assoc (into {}
                          (filter
                           (fn [[_id element]]
                             (and (or (not= (:entity element)    entity)
                                      (not= (:attribute element) attribute))
                                  (not= value (:value element)))))
                          elements)
                    id
                    (->Element entity attribute value))))))

(defmethod -interpret-op :remove
  [{:keys [elements] :as agg} id {:keys [data] :as op}]
  (let [{:keys [entity attribute]} data]
    (assoc agg :elements (into {}
                               (filter
                                (fn [[_id element]]
                                  (or (not= (:entity element)    entity)
                                      (not= (:attribute element) attribute))))
                               elements))))

(defmethod -interpret-op :insert
  [{:keys [list-links] :as agg} id {{prev :after} :data :as op}]
  (let [next (get list-links prev)]
    (if next
      (assoc agg
             :list-links
             (-> list-links
                 (dissoc prev)
                 (assoc prev id
                        id next)))
      agg)))

(defmethod -interpret-op :make-list
  [agg id _op]
  (-> agg
      (assoc-in [:list-links id] list-end-sigil)
      (assoc-in [:elements id] [])))

(defmethod -interpret-op :make-map
  [agg id _op]
  (assoc-in agg [:elements id] {}))

(defmethod -interpret-op :make-value
  [agg id op]
  (assoc-in agg [:elements id] (-> op :data :value)))

  (defn interpret
    ([opset]
     (reduce-kv -interpret-op
                (->Interpretation {} {})
                opset))
    ([interpretation ops]
     (reduce-kv -interpret-op
                interpretation
                ops)))
