(ns converge.interpret
  "Functions for interpreting an OpSet as per sections 3.2 and 5.2 of
  the [OpSets paper](https://arxiv.org/pdf/1805.04263.pdf)"
  (:require [clojure.set :as set]
            [converge.opset :as opset])
  #?(:clj (:import [converge.opset Id])))

#?(:clj (set! *warn-on-reflection* true))

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


(defrecord Element [#?@(:cljs [^clj object]
                        :clj  [^Id object])
                    key
                    #?@(:cljs [^clj value]
                        :clj  [^Id value])])

(defrecord Interpretation [elements list-links])

(def list-end-sigil ::list-end)

(defn ancestor
  [elements]
  (transitive-closure
   (into #{} (map (juxt :object :value) elements))))

(defmulti -interpret-op
  (fn [_agg _id {action :action :as op}] action)
  :default ::default)

(defmethod -interpret-op ::default
  [agg _id _op]
  agg)

(defmethod -interpret-op :assign
  [{:keys [elements] :as agg} id {:keys [data] :as op}]
  (let [{:keys [object key value]} data]
    ;; Skip operations that would introduce a cycle from decendent
    ;; (value) to ancestor (object), per Section 5.2
    (if (contains? (ancestor elements) [value object])
      agg
      (assoc agg
             :elements
             (assoc (into {}
                          (filter
                           (fn [[_id element]]
                             (and (or (not= (:object element) object)
                                      (not= (:key element)    key))
                                  (not= value (:value element))))
                           elements))
                    id
                    (->Element object key value))))))

(defmethod -interpret-op :remove
  [{:keys [elements] :as agg} id {:keys [data] :as op}]
  (let [{:keys [object key]} data]
    (assoc agg :elements (into {}
                               (filter
                                (fn [[_id element]]
                                  (or (not= (:object element) object)
                                      (not= (:key element)    key)))
                                elements)))))

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
  (assoc-in agg [:list-links id] list-end-sigil))

(defn interpret
  [opset]
  (reduce-kv -interpret-op
             (->Interpretation {} {})
             opset))
