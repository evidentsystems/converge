(ns converge.opset
  "Data types and functions to implement and manage OpSets: lamport-like `Id`s,
  `Op`(eration)s as per section 3.1 of the
  [OpSet paper](https://arxiv.org/pdf/1805.04263.pdf), and `OpSet`s or
  totally ordered maps of id -> op."
  (:refer-clojure :exclude [remove list #?(:cljs uuid)])
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

(set! *warn-on-reflection* true)

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
  [{:keys [counter actor] :as i}]
  (make-id actor (inc counter)))

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
  [opset actor]
  (first (avl/nearest opset < (make-id actor Long/MAX_VALUE))))

(defn next-id
  [opset actor]
  (let [latest (latest-id opset actor)]
    (if (= root-id latest)
      (make-id actor)
      (successor-id latest))))

(defprotocol ValueIntoOps
  (value-to-ops [value actor value-id next-id]))

(extend-protocol ValueIntoOps
  #?(:cljs default
     :clj  java.lang.Object)
  (value-to-ops
    [value _ value-id _next-id]
    [[value-id (make-value value)]])

  nil
  (value-to-ops
    [value _ value-id _next-id]
    [[value-id (make-value value)]])

  #?(:cljs IMap
     :clj  java.util.Map)
  (value-to-ops
    [value actor map-id next-id]
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
                  {:id  next-id
                   :ops initial-ops}
                  value))))

  #?(:cljs ISequential
     :clj  java.util.List)
  (value-to-ops
    [value actor list-id next-id]
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
               {:id      next-id
                :tail-id list-id
                :ops     initial-ops}
               value)))))

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
  (let [container    (or (util/safe-get-in old (butlast path))
                         old)
        container-id (util/get-id container)
        key          (if (map? container)
                       (last path)
                       (util/get-insertion-id container (last path)))]
    [[id (remove container-id key)]]))

(defmethod edit-to-ops :r
  [[path _ new-value :as edit] old actor id]
  (if (seq path)
    ;; TODO: Check for existing IDs?
    (insert-and-or-assign edit old actor id)
    ;; old value is empty
    (value-to-ops new-value actor root-id id)))

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
