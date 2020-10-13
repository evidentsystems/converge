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
(ns converge.opset
  "Data types and functions to implement and manage OpSets: lamport-like `Id`s,
  `Op`(eration)s as per section 3.1 of the
  [OpSet paper](https://arxiv.org/pdf/1805.04263.pdf), and `OpSet`s or
  totally ordered maps of id -> op."
  (:refer-clojure :exclude [remove #?(:cljs uuid)])
  (:require #?@(:clj
                [[clj-uuid :as uuid]]
                :cljs
                [[uuid :as uuid]])
            [clojure.data :as data]
            [clojure.data.avl :as avl]
            [clojure.zip :as zip]
            [editscript.core :as editscript]
            [editscript.edit :as edit]
            [converge.util :as util])
  #?(:clj (:import java.util.UUID)))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(declare id?)

(defrecord Id [^UUID actor ^long counter]
  #?(:clj  Comparable
     :cljs IComparable)
  (#?(:clj  compareTo
      :cljs -compare)
    [_ other]
    (assert (id? other))
    (compare
     [counter actor]
     [(:counter other) (:actor other)])))

(defn make-id
  ([]
   (make-id (util/uuid)))
  ([actor]
   (make-id actor 0))
  ([actor counter]
   (assert (nat-int? counter) "The `counter` of an Id must be an integer")
   (assert (or (nil? actor) (uuid? actor)) "The `actor` of an Id must be a UUID")
   (->Id actor counter)))

(defn id?
  [o]
  (instance? Id o))

(def root-id
  (make-id nil 0))

(defn successor-id
  ([id]
   (successor-id id (:actor id)))
  ([{:keys [counter] :as i} actor]
   (make-id actor (inc counter))))

(defrecord Op [action data])

(defn op
  ([action]
   (op action nil))
  ([action data]
   (assert (keyword? action) "The `action` of an Op must be a keyword")
   (assert (or (nil? data) (map? data)) "The `data` of an Op, if provided, must be a map")
   (->Op action data)))

;;;; Operations

(defn make-map
  []
  (op :make-map))

(defn make-list
  []
  (op :make-list))

(defn make-value
  [val]
  (op :make-value {:value val}))

(defn insert
  [after]
  (assert (id? after) "`after` must be an Id")
  (op :insert {:after after}))

(defn assign
  [object k v]
  (assert (id? object) "`object` must be an Id")
  (op :assign {:object object :key k :value v}))

(defn remove
  [object k]
  (assert (id? object) "`object` must be an Id")
  (op :remove {:object object :key k}))

(defn opset
  "An opset is a sorted map of Id -> Op"
  ([]
   (avl/sorted-map))
  ([& id-ops]
   (apply avl/sorted-map id-ops)))

(defn latest-id
  [opset]
  (some-> opset last key))

(defn next-id
  [opset actor]
  (if-let [latest (latest-id opset)]
    (successor-id latest actor)
    root-id))

(declare value-to-ops)
(defn map-to-value
  [value actor map-id start-id]
  (let [initial-ops
        (if (= root-id map-id)
          [] ;; TODO: do we need this if we remove all special case init in convergent-ref?
          [[map-id (make-map)]])]
    (:ops
     (reduce-kv (fn [{:keys [id] :as agg} k v]
                  (let [value-id  id
                        assign-id (successor-id value-id)
                        value-ops (value-to-ops v actor value-id (successor-id assign-id))]
                    (-> agg
                        (update :ops
                                into
                                (apply vector
                                       (first value-ops)
                                       [assign-id (assign map-id k value-id)]
                                       (next value-ops)))
                        (assoc :id (successor-id
                                    (if (next value-ops)
                                      (first (last value-ops))
                                      assign-id))))))
                {:id  start-id
                 :ops initial-ops}
                value))))

(defn sequential-to-value
  [value actor list-id start-id]
  (let [initial-ops
        (if (= root-id list-id)
          [] ;; TODO: do we need this if we remove all special case init in convergent-ref?
          [[list-id (make-list)]])]
    (:ops
     (reduce (fn [{:keys [id tail-id] :as agg} v]
               (let [insert-id id
                     value-id  (successor-id insert-id)
                     assign-id (successor-id value-id)
                     value-ops (value-to-ops v actor value-id (successor-id assign-id))]
                 (-> agg
                     (update :ops
                             into
                             (apply vector
                                    [insert-id (insert tail-id)]
                                    (first value-ops)
                                    [assign-id (assign list-id insert-id value-id)]
                                    (next value-ops)))
                     (assoc :id (successor-id
                                 (if (next value-ops)
                                   (first (last value-ops))
                                   assign-id))
                            :tail-id insert-id))))
             {:id      start-id
              :tail-id list-id
              :ops     initial-ops}
             value))))

(defn value-to-ops
  [value actor value-id start-id]
  (cond
    (map? value)
    (map-to-value value actor value-id start-id)

    (sequential? value)
    (sequential-to-value value actor value-id start-id)

    :else
    [[value-id (make-value value)]]))

(defmulti edit-to-ops
  "Returns a vector of tuples of [id op] that represent the given Editscript edit."
  (fn [edit _old-value _actor _id] (nth edit 1))
  :default ::default)

(defmethod edit-to-ops ::default
  [edit _old _actor _id]
  (throw (ex-info "Unknown edit operation" {:edit edit})))

(defn insert-and-or-assign
  [[path _ new-value :as edit] old actor id]
  (let [container    (or (util/safe-get-in old (butlast path))
                         old)
        container-id (util/get-id container)]
    (if (map? container)
      (let [value-id  id
            assign-id (successor-id value-id)
            value-ops (value-to-ops new-value actor value-id (successor-id assign-id))]
        (apply vector
               (first value-ops)
               [assign-id (assign container-id (last path) value-id)]
               (next value-ops)))
      (let [insert-id id
            value-id  (successor-id insert-id)
            assign-id (successor-id value-id)
            value-ops (value-to-ops new-value actor value-id (successor-id assign-id))
            after-id  (util/get-insertion-id container (last path))]
        (apply vector
               [insert-id (insert after-id)]
               (first value-ops)
               [assign-id (assign container-id insert-id value-id)]
               value-ops)))))

(defmethod edit-to-ops :+
  [edit old actor id]
  (insert-and-or-assign edit old actor id))

(defmethod edit-to-ops :-
  [[path :as edit] old actor id]
  (let [container    (util/safe-get-in old (butlast path))
        container-id (util/get-id container)
        key          (if (map? container)
                       (last path)
                       (util/get-insertion-id container (last path)))]
    [[id (remove container-id key)]]))

(defmethod edit-to-ops :r
  [[path _ new-value :as edit] old actor id]
  (if (empty? old)
    (value-to-ops new-value actor root-id id)
    ;; TODO: Check for existing IDs?
    (let [object    (util/safe-get-in old path)
          object-id (util/get-id object)]
      (if (and (coll? new-value)
               (empty? new-value))
        (:ops
         (reduce (fn [{:keys [ops id] :as agg} k]
                   (-> agg
                       (update :ops conj [id (remove object-id k)])
                       (assoc :id (successor-id id))))
                 {:id  id
                  :ops []}
                 (keys old)))
        (insert-and-or-assign edit old actor id)))))

(defn ops-from-diff
  [opset actor old-value new-value]
  (:ops
   (reduce (fn [{:keys [ops id] :as agg} edit]
             (let [new-ops (into ops
                                 (edit-to-ops edit
                                              old-value
                                              actor
                                              id))]
               (assoc agg
                      :ops new-ops
                      :id  (next-id new-ops actor))))
           {:ops (avl/sorted-map)
            :id  (next-id opset actor)}
           (edit/get-edits
            (editscript/diff old-value new-value)))))

(defn add-ops-from-diff
  [opset actor old-value new-value]
  (reduce (fn [agg edit]
            (into agg
                  (edit-to-ops edit
                               old-value
                               actor
                               (next-id agg actor))))
          opset
          (edit/get-edits
           (editscript/diff old-value new-value))))

(comment

  (def a {:empty-m {}
          :empty-l []
          :a       :key
          :another {:nested {:key [1 2 3]}}
          :a-list  [:foo "bar" 'baz {:nested :inalist}]
          :a-set   #{1 2 3 4 5}})

  (def a {:foo :bar :baz :quux :a-list [:item2 :item1]})

  (def o (atom (opset root-id (make-map))))
  (def i (make-id))

  (swap! o add-ops-from-diff (:actor i) {} a)
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
