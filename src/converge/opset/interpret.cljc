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
(ns converge.opset.interpret
  "Functions for interpreting an OpSet as per sections 3.2 and 5.2 of
  the [OpSets paper](https://arxiv.org/pdf/1805.04263.pdf)"
  (:require [clojure.set :as set]
            [converge.opset.ops :as ops])
  #?(:clj (:import [converge.core Id])))

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
   (into #{}
         (map (juxt :entity :value))
         (vals elements))))

(defmulti -interpret-op
  (fn [_agg _id op] (:action op))
  :default ::default)

(defmethod -interpret-op ::default
  [agg _id _op]
  agg)

;; TODO: test opset log hash prior to this id matches hash stored in this op
(defmethod -interpret-op ops/SNAPSHOT
  [agg _id {{{:keys [elements list-links]} :interpretation} :data}]
  (assoc agg
         :elements   (transient elements)
         :list-links (transient list-links)))

(defmethod -interpret-op ops/ASSIGN
  [{:keys [elements] :as agg} id {:keys [data]}]
  (let [{:keys [entity attribute value]} data

        elements* (persistent! elements)]
    ;; Skip operations that would introduce a cycle from decendent
    ;; (value) to ancestor (entity), per Section 5.2
    (if false #_(contains? (ancestor elements*) [value entity])
      agg
      (assoc agg
             :elements
             (transient
              (into {id (->Element entity attribute value)}
                    (filter
                     (fn [[_id element]]
                       (and (or (not= (:entity element)    entity)
                                (not= (:attribute element) attribute))
                            (not= value (:value element)))))
                    elements*))))))

(defmethod -interpret-op ops/REMOVE
  [{:keys [elements] :as agg} _id {:keys [data]}]
  (let [{:keys [entity attribute]} data]
    (assoc agg
           :elements
           (transient
            (into {}
                  (filter
                   (fn [[_id element]]
                     (or (not= (:entity element)    entity)
                         (not= (:attribute element) attribute))))
                  (persistent! elements))))))

(defmethod -interpret-op ops/INSERT
  [{:keys [list-links] :as agg} id {{prev :after} :data}]
  (let [next (get list-links prev)]
    (if next
      (assoc agg
             :list-links
             (-> list-links
                 (dissoc! prev)
                 (assoc! prev id
                         id next)))
      agg)))

(defmethod -interpret-op ops/MAKE_LIST
  [agg id _op]
  (-> agg
      (update :list-links assoc! id list-end-sigil)
      (update :elements assoc! id [])))

(defmethod -interpret-op ops/MAKE_MAP
  [agg id _op]
  (update agg :elements assoc! id {}))

(defmethod -interpret-op ops/MAKE_VALUE
  [agg id op]
  (update agg :elements assoc! id (-> op :data :value)))

(defn interpret
  ([opset-log]
   (interpret (->Interpretation {} {}) opset-log))
  ([{:keys [elements list-links] :as _interpretation} ops]
   (let [{elements*   :elements
          list-links* :list-links}
         (reduce-kv -interpret-op
                    {:elements   (transient elements)
                     :list-links (transient list-links)}
                    ops)]
     (->Interpretation
      (persistent! elements*)
      (persistent! list-links*)))))
