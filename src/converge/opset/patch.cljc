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
(ns converge.opset.patch
  (:require [clojure.data.avl :as avl]
            [editscript.core :as e]
            [converge.core :as core]
            [converge.util :as util]
            [converge.opset.edn :as edn]
            [converge.opset.interpret :as interpret]
            [converge.opset.ops :as ops]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;;; Util

(defn get-id
  [o]
  (some-> o meta :converge/id))

(defn get-insertion-id
  [o n]
  (some-> o meta :converge/insertions (util/safe-get n)))

;;;; Value to Ops

(defn add-key
  [{:keys               [log actor]
    {:keys [key-cache]} :interpretation
    :as                 context}
   k]
  (let [key-id (or (get key-cache k)
                   (core/next-id log actor))
        ops      {key-id (ops/make-key k)}]
    (-> context
        (update :log merge ops)
        (update :interpretation interpret/interpret ops)
        (vector key-id))))

(defmulti add-value
  (fn [_context value] (util/get-type value))
  :default ::default)

(defmethod add-value ::default
  [_ value]
  (throw
   (ex-info "Don't know how to add this value to the opset"
            {:value value})))

(defn add-assign-op
  ([ctx entity-id key-id]
   (add-assign-op ctx entity-id key-id nil))
  ([{:keys [log actor] :as context} entity-id key-id val-id]
   (let [assign-id (core/next-id log actor)
         ops {assign-id
              (ops/assign entity-id key-id val-id)}]
     (-> context
         (update :log merge ops)
         (update :interpretation interpret/interpret ops)
         (vector assign-id)))))

(defn add-insert-op
  [{:keys [log actor] :as context} after-id]
  (let [insert-id (core/next-id log actor)
        ops       {insert-id (ops/insert after-id)}]
    (-> context
        (update :log merge ops)
        (update :interpretation interpret/interpret ops)
        (vector insert-id))))

(defn populate-list
  [ctx* list-id the-list]
  (loop [ctx       ctx*
         after-id  list-id
         items     the-list]
    (if-some [item (first items)]
      (let [[ctx1 value-id]
            (add-value ctx item)

            [ctx2 insert-id]
            (add-insert-op ctx1 after-id)

            ctx3
            (util/first-indexed
             (add-assign-op ctx2 list-id insert-id value-id))]
        (recur ctx3
               insert-id
               (next items)))
      ctx)))

(defmethod add-value :val
  [{:keys                 [log actor]
    {:keys [value-cache]} :interpretation
    :as                   context}
   value]
  (let [value-id (or (get value-cache value)
                     (core/next-id log actor))
        ops      {value-id (ops/make-value value)}]
    (-> context
        (update :log merge ops)
        (update :interpretation interpret/interpret ops)
        (vector value-id))))

(defmethod add-value :map
  [{:keys [log actor] :as context} the-map]
  (let [[map-id op]
        (if-let [existing-id (get-id the-map)]
          [existing-id nil]
          [(core/next-id log actor)
           (ops/make-map)])

        ops
        (when op {map-id op})]
    [(reduce-kv
      (fn [ctx k v]
        (let [[ctx1 key-id]
              (add-key ctx k)
              [ctx2 value-id]
              (add-value ctx1 v)]
          (util/first-indexed
           (add-assign-op ctx2 map-id key-id value-id))))
      (-> context
          (update :log merge ops)
          (update :interpretation
                  interpret/interpret
                  ops))
      the-map)
     map-id]))

(defmethod add-value :vec
  [{:keys [log actor] :as context} the-vector]
  (let [[vector-id op]
        (if-let [existing-id (get-id the-vector)]
          [existing-id nil]
          [(core/next-id log actor)
           (ops/make-vector)])

        ops
        (when op {vector-id op})]
    [(populate-list (-> context
                        (update :log merge ops)
                        (update :interpretation
                                interpret/interpret
                                ops))
                    vector-id
                    the-vector)
     vector-id]))

(defmethod add-value :set
  [{:keys [log actor] :as context} the-set]
  (let [[set-id op]
        (if-let [existing-id (get-id the-set)]
          [existing-id nil]
          [(core/next-id log actor)
           (ops/make-set)])

        ops
        (when op {set-id op})]
    [(reduce
      (fn [ctx k]
        (let [[ctx1 key-id]
              (add-key ctx k)]
          (util/first-indexed
           (add-assign-op ctx1 set-id key-id))))
      (-> context
          (update :log merge ops)
          (update :interpretation
                  interpret/interpret
                  ops))
      the-set)
     set-id]))

(defmethod add-value :lst
  [{:keys [log actor] :as context} the-list]
  (let [[list-id op]
        (if-let [existing-id (get-id the-list)]
          [existing-id nil]
          [(core/next-id log actor)
           (ops/make-list)])

        ops
        (when op {list-id op})]
    [(populate-list (-> context
                        (update :log merge ops)
                        (update :interpretation
                                interpret/interpret
                                ops))
                    list-id
                    the-list)
     list-id]))

(defn create-value-and-assign-key-ops
  [ops map-id k v key-id]
  (let [ops-after-key (assoc ops key-id (ops/make-key k))
        val-id        (core/successor-id key-id)
        ops-after-val (value-to-ops ops-after-key val-id v)]
    (assign-key-ops ops-after-val map-id (core/next-id ops-after-val) key-id val-id)))

(defn assign-set-member-ops
  [ops set-id assign-id member]
  (assoc ops assign-id (ops/assign set-id member)))

(defn insert-list-item-ops
  [ops list-id insert-id after-id item-id]
  (let [ops-after-insert (assoc ops insert-id (ops/insert after-id))
        assign-id        (core/next-id ops-after-insert)]
    (assoc ops-after-insert assign-id (ops/assign list-id insert-id item-id))))

(defn create-and-insert-list-item-ops
  [ops list-id insert-id after-id item]
  (let [ops-after-insert (assoc ops insert-id (ops/insert after-id))
        item-id          (core/next-id ops-after-insert)
        ops-after-item   (value-to-ops ops-after-insert item-id item)
        assign-id        (core/next-id ops-after-item)]
    (assoc ops-after-item assign-id (ops/assign list-id insert-id item-id))))

(defn remove-key-ops
  [ops entity-id remove-id key-id]
  (assoc ops remove-id (ops/remove entity-id key-id)))

(defmethod value-to-ops :map
  [ops map-id the-map]
  (reduce-kv (fn [agg k v]
               (create-value-and-assign-key-ops
                agg map-id k v (core/next-id agg)))
             (assoc ops map-id (ops/make-map))
             the-map))

(defmethod value-to-ops :vec
  [ops vector-id the-vector]
  (populate-list-ops (assoc ops vector-id (ops/make-vector))
                     vector-id
                     the-vector
                     (core/successor-id vector-id)))

(defmethod value-to-ops :set
  [ops set-id the-set]
  (reduce (fn [agg item]
            (assign-set-member-ops agg set-id (core/next-id agg) item))
          (assoc ops set-id (ops/make-set))
          the-set))

(defmethod value-to-ops :lst
  [ops list-id the-list]
  (populate-list-ops (assoc ops list-id (ops/make-list))
                     list-id
                     the-list
                     (core/successor-id list-id)))

;;;; Edit to Ops

(defmulti -edit-to-ops
  "Returns a vector of tuples of [id op] that represent the given Editscript edit."
  (fn [_ops _actor edit entity]
    [(nth edit 1)
     (util/get-type entity)]))

(defmethod -edit-to-ops :default
  [_ops _actor edit entity]
  (throw
   (ex-info "Unknown edit operation"
            {:edit   edit
             :entity entity
             :op     (nth edit 1)
             :type   (util/get-type entity)})))

;;;; Add

(defmethod -edit-to-ops [:+ :map]
  [ops actor [path _ v] the-map]
  (create-value-and-assign-key-ops
   ops
   (get-id the-map)
   (util/last-indexed path)
   v
   (core/next-id ops actor)))

(defmethod -edit-to-ops [:+ :vec]
  [ops actor [path _ item] the-vector]
  (let [entity-id (get-id the-vector)]
    (create-and-insert-list-item-ops
     ops
     entity-id
     (core/next-id ops actor)
     (or (some->> path
                  util/last-indexed
                  dec
                  (get-insertion-id the-vector))
         entity-id)
     item)))

(defmethod -edit-to-ops [:+ :set]
  [ops actor [_ _ item] the-set]
  (assign-set-member-ops ops (get-id the-set) (core/next-id ops actor) item))

(defmethod -edit-to-ops [:+ :lst]
  [ops actor [path _ item] the-list]
  (let [entity-id (get-id the-list)]
    (create-and-insert-list-item-ops
     ops
     entity-id
     (core/next-id ops actor)
     (or (some->> path
                  util/last-indexed
                  dec
                  (get-insertion-id the-list))
         entity-id)
     item)))

;;;; Remove

(defmethod -edit-to-ops [:- :map]
  [ops actor [path] the-map]
  (remove-key-ops ops
                  (get-id the-map)
                  (core/next-id ops actor)
                  (util/last-indexed path)))

(defmethod -edit-to-ops [:- :vec]
  [ops actor [path] the-vector]
  (remove-key-ops ops
                  (get-id the-vector)
                  (core/next-id ops actor)
                  (->> path
                       util/last-indexed
                       (get-insertion-id the-vector))))

(defmethod -edit-to-ops [:- :set]
  [ops actor [path] the-set]
  (remove-key-ops ops
                  (get-id the-set)
                  (core/next-id ops actor)
                  (util/last-indexed path)))

(defmethod -edit-to-ops [:- :lst]
  [ops actor [path] the-list]
  (remove-key-ops ops
                  (get-id the-list)
                  (core/next-id ops actor)
                  (->> path
                       util/last-indexed
                       (get-insertion-id the-list))))

;;;; Replace

(defn map-diff-ops
  [ops* actor map-id old-map new-map]
  (reduce (fn [ops k]
            (cond
              (and (contains? old-map k)
                   (not (contains? new-map k)))
              (remove-key-ops ops
                              map-id
                              (core/next-id ops actor)
                              k)

              (and (not (contains? old-map k))
                   (contains? new-map k))
              (let [value         (get new-map k)
                    key-id        (core/next-id ops actor)
                    ops-after-key (assoc ops key-id (ops/make-key k))]
                (if-let [val-id (get-id value)]
                  (assign-key-ops
                   ops-after-key
                   map-id
                   (core/successor-id key-id)
                   key-id
                   val-id)
                  (create-value-and-assign-key-ops
                   ops-after-key
                   map-id
                   key-id
                   value
                   (core/successor-id key-id))))

              (and (contains? old-map k)
                   (contains? new-map k)
                   (not= (get old-map k)
                         (get new-map k)))
              ops

              :else
              ops))
          ops*
          (into #{} (concat (keys old-map) (keys new-map)))))

;; TODO: Can we make this better?  Preserve existing insertions/assignments/values?
(defn list-diff-ops
  [ops* actor list-id old-list new-list]
  (let [ops-after-removals
        (loop [ops ops*
               items old-list
               i    0]
          (if (first items)
            (recur (remove-key-ops ops
                                   list-id
                                   (core/next-id ops actor)
                                   (get-insertion-id old-list i))
                   (next items)
                   (inc i))
            ops))]
    (loop [ops       ops-after-removals
           insert-id (core/next-id ops-after-removals actor)
           after-id  list-id
           items     new-list]
      (if-let [item (first items)]
        (let [next-ops (if-let [id (get-id item)]
                         (insert-list-item-ops
                          ops list-id insert-id after-id id)
                         (create-and-insert-list-item-ops
                          ops list-id insert-id after-id item))]
          (recur next-ops
                 (core/next-id next-ops)
                 insert-id
                 (next new-list)))
        ops))))

(defn set-diff-ops
  [ops* actor set-id old-set new-set]
  (reduce (fn [ops k]
            (cond
              (and (contains? old-set k)
                   (not (contains? new-set k)))
              (remove-key-ops ops
                              set-id
                              (core/next-id ops actor)
                              k)

              (and (not (contains? old-set k))
                   (contains? new-set k))
              (assign-set-member-ops ops
                                     set-id
                                     (core/next-id ops actor)
                                     k)

              :else ops))
          ops*
          (into old-set new-set)))

(defn replace-in-list-ops
  [ops actor path new-value the-list]
  (if (empty? path)
    ;; Replacing the root list...
    (if (= (util/get-type new-value) :list)
      ;; ...with minimal ops while retaining list identity, since new value is also a list
      (list-diff-ops ops actor (get-id the-list) the-list new-value)
      ;; ...entirely with a new value with :root? true
      (let [value-id (core/next-id ops actor)
            ops*     (value-to-ops ops value-id new-value)]
        (assoc ops* value-id (assoc-in (get ops* value-id) [:data :root?] true))))
    ;; Replacing a key within this list...
    (let [k         (util/last-indexed path)
          old-value (util/safe-get the-list k)]
      (if (= (util/get-type old-value)
             (util/get-type new-value))
        ;; ...with minimal ops while attempting to retain value
        ;; identity, since new value is same type
        (case (util/get-type old-value)
          :map (map-diff-ops ops actor (get-id old-value) old-value new-value)
          (:vec :lst) (list-diff-ops ops actor (get-id old-value) old-value new-value)
          :set (set-diff-ops ops actor (get-id old-value) old-value new-value)
          ;; else replace a primitive type
          (create-value-and-assign-key-ops ops
                                           (get-id the-list)
                                           (get-insertion-id the-list k)
                                           new-value
                                           (core/next-id ops actor)))
        ;; TODO: attempt to retain inner values when converting from one collection type to another?
        ;; TODO: more broadly, should we cache and re-use scalar (not including tracking text/string) values?
        ;; ... with an entirely new value
        (create-value-and-assign-key-ops ops
                                         (get-id the-list)
                                         (get-insertion-id the-list k)
                                         new-value
                                         (core/next-id ops actor))))))

(defmethod -edit-to-ops [:r :map]
  [ops actor [path _ new-value] the-map]
  (if (empty? path)
    ;; Replacing the root map...
    (if (= (util/get-type new-value) :map)
      ;; ...with minimal ops while retaining map identity, since new value is also a map
      (map-diff-ops ops actor (get-id the-map) the-map new-value)
      ;; ...entirely with a new value with :root? true
      (let [value-id (core/next-id ops actor)
            ops*     (value-to-ops ops value-id new-value)]
        (assoc ops* value-id (assoc-in (get ops* value-id) [:data :root?] true))))
    ;; Replacing a key within this map...
    (let [k         (util/last-indexed path)
          old-value (get the-map k)]
      (if (= (util/get-type old-value)
             (util/get-type new-value))
        ;; ...with minimal ops while attempting to retain value
        ;; identity, since new value is same type
        (case (util/get-type old-value)
          :map (map-diff-ops ops actor (get-id old-value) old-value new-value)
          (:vec :lst) (list-diff-ops ops actor (get-id old-value) old-value new-value)
          :set (set-diff-ops ops actor (get-id old-value) old-value new-value)
          ;; else replace a primitive type
          (let [key-id (core/next-id ops actor)]
            (create-value-and-assign-key-ops (assoc ops key-id (ops/make-key k))
                                             (get-id the-map)
                                             key-id
                                             new-value
                                             (core/successor-id key-id))))
        ;; TODO: attempt to retain inner values when converting from one collection type to another?
        ;; TODO: more broadly, should we cache and re-use scalar (not including tracking text/string) values?
        ;; ... with an entirely new value
        (let [key-id (core/next-id ops actor)]
          (create-value-and-assign-key-ops (assoc ops key-id (ops/make-key k))
                                           (get-id the-map)
                                           key-id
                                           new-value
                                           (core/successor-id key-id)))))))

(defmethod -edit-to-ops [:r :vec]
  [ops actor [path _ value] the-vector]
  (replace-in-list-ops ops actor path value the-vector))

(defmethod -edit-to-ops [:r :set]
  [ops actor [path _ new-value] the-set]
  (if (empty? path)
    ;; Replacing the root set...
    (if (= (util/get-type new-value) :set)
      ;; ...with minimal ops while retaining set identity, since new value is also a set
      (set-diff-ops ops actor (get-id the-set) the-set new-value)
      ;; ...entirely with a new value with :root? true
      (let [value-id (core/next-id ops actor)
            ops*     (value-to-ops ops value-id new-value)]
        (assoc ops* value-id (assoc-in (get ops* value-id) [:data :root?] true))))
    ;; We don't replace keys within set...
    ops))

(defmethod -edit-to-ops [:r :lst]
  [ops actor [path _ value] the-list]
  (replace-in-list-ops ops actor path value the-list))

;; TODO: incorporate root? true into add-value
(defmethod -edit-to-ops [:r :val]
  [ops actor [path _ value] _entity]
  (let [id   (core/next-id ops actor)
        ops* (value-to-ops ops id value)]
    (if (empty? path)
      (if-let [op (get ops* id)]
        (assoc ops* id (assoc-in op [:data :root?] true))
        ops*)
      ops*)))

(defn edit-to-ops
  [{:keys [value] :as context} [path :as edit]]
  (-edit-to-ops context
                edit
                (util/safe-get-in value (util/safe-pop path))))

(defn make-patch
  [log-orig interpretation actor old-value new-value]
  (let [context (reduce edit-to-ops
                        {:actor actor
                         :log   log-orig
                         :value old-value
                         :interpretation
                         (or interpretation (interpret/interpret log-orig))}
                        (e/get-edits
                         (e/diff old-value new-value)))
        ops     (avl/subrange (:log context) > (core/latest-id log-orig))]
    (when-not (empty? ops)
      (core/map->Patch
       {:source         (-> log-orig
                            core/ref-root-data-from-log
                            :id)
        :ops            ops
        :interpretation (:interpretation context)
        :value          (:value context)}))))

(comment

  (def ref-id
    (util/uuid))

  (def creator
    (util/uuid))

  (def log1
    (core/make-log
     (core/make-id)
     (core/root-op ref-id creator :opset)))

  (edn/edn (interpret/interpret log1))

  (def actor (util/uuid))

  (def val1 '{:empty-m {}, :empty-l [], :a :key, :another {:nested {:key [1 2 3]}}, :a-list [:foo "bar" baz {:nested :inalist}], :a-set #{1 4 3 2 5}})

  (def patch1 (make-patch log1 nil actor nil val1))

  patch1

  (def log2 (merge log1 (:ops patch1)))

  (edn/edn (interpret/interpret log2))

  (def val2 '{:empty-m {}, :empty-l [], :a :key, :another {:nested {:key [1 2 3]}}, :a-list [:foo "bar baz" baz {:nested :inalist}], :a-set #{1 4 3 2 5}})

  (def patch2 (make-patch log2 nil actor (edn/edn (interpret/interpret log2)) val2))

  patch2

  (def log3 (merge log2 (:ops patch2)))

  (edn/edn (interpret/interpret log3))

  (def r (converge.api/ref [[0]] :backend :opset))
  @r
  (reset! r [[]])

  (def r (converge.api/ref [0] :backend :opset))
  @r
  (reset! r [])
  (meta @r)

  (reset! r #{0})

  (e/diff #{[0]} #{0})

;;;; Replace in Map cases

  ;; Case 1: replacing a value with another of a different type
  (e/get-edits (e/diff {:foo :bar} [[:foo :bar]]))
  :->
  [[[] :r [[:foo :bar]]]]

  (e/get-edits (e/diff {:outer {:foo :bar}} {:outer [[:foo :bar]]}))
  :->
  [[[:outer] :r [[:foo :bar]]]]

  ;; Case 2a: replacing empty collections of the same type with a non-empty collection
  (e/get-edits (e/diff {:outer {}} {:outer {:foo :bar}}))
  :->
  [[[:outer] :r {:foo :bar}]]

  (e/get-edits (e/diff #{1 2 3 4 5} #{1 2 3}))

  ;; Case 2b: "sufficiently different" collections of the same type.
  ;; For maps and sets, seems to be when old and new share less than
  ;; (quot (count x) 2) entries in common
  ;; For lists
  (e/get-edits (e/diff {1 2 3 4 5 6 7 8 9 10} {1 2 3 4}))
  :->
  [[[5] :-] [[7] :-] [[9] :-]]

  (e/get-edits (e/diff {:foo {1 2 3 4 5 6 7 8 9 10}} {:foo {1 2 3 5}}))
  :->
  [[[] :r {1 2, 3 5}]]

  ;; Case 2c: replacing non-empty collections of the same type with empty collections
  (e/get-edits (e/diff {:foo :bar} {}))
  :->
  [[[] :r {}]]

  ;;;; Replace in List cases

  ;; Case 1: replacing a value with another of a different type
  (e/get-edits (e/diff {:foo '(:bar :baz :quux)} {:foo #{:bar :baz :quux}}))

  (e/get-edits (e/diff {:foo '(:bar :baz :quux)}
                       {:foo '(:a :bunch :more :another :bar :baz :first :quux :second :third :fourth)}))
  (e/get-edits (e/diff {:foo '(:bar :baz :quux)} {:foo '(:bar :quux)}))

  (def a [2 {:a 42} 3 {:b 4} {:c 29}])
  (def b [{:a 5} {:b 5}])

  (e/diff a b {:algo :quick})

  ;; end
  )
