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

;;;; Value to Ops

(defmulti value-to-ops
  (fn [_ops _value-id value] (util/get-type value))
  :default ::default)

(defmethod value-to-ops ::default
  [ops value-id value]
  (assoc ops value-id (ops/make-value value)))

(defn assign-map-key-ops
  [ops map-id key-id k v]
  (let [ops-after-key (value-to-ops ops key-id k)
        val-id        (core/next-id ops-after-key)
        ops-after-val (value-to-ops ops-after-key val-id v)
        assign-id     (core/next-id ops-after-val)]
    (assoc ops-after-val assign-id (ops/assign map-id key-id val-id))))

(defn assign-set-member-ops
  [ops map-id member-id member]
  (let [ops-after-member (value-to-ops ops member-id member)
        assign-id        (core/next-id ops-after-member)]
    (assoc ops-after-member assign-id (ops/assign map-id member-id))))

(defn insert-list-item-ops
  [ops list-id insert-id after-id item]
  (let [ops-after-insert (assoc ops insert-id (ops/insert after-id))
        item-id          (core/next-id ops-after-insert)
        ops-after-item   (value-to-ops ops-after-insert item-id item)
        assign-id        (core/next-id ops-after-item)]
    (assoc ops-after-item assign-id (ops/assign list-id insert-id item-id))))

(defn reassign-existing-key-ops
  [ops entity-id key-id value value-id]
  (let [agg-after-val (value-to-ops ops value-id value)
        assign-id     (core/next-id agg-after-val)]
    (assoc agg-after-val assign-id (ops/assign entity-id key-id value-id))))

(defn remove-key-ops
  [ops entity-id remove-id key-id]
  (assoc ops remove-id (ops/remove entity-id key-id)))

(defn populate-map-ops
  [ops* map-id the-map start-id]
  (loop [ops     ops*
         key-id  start-id
         entries the-map]
    (if-let [entry (first entries)]
      (let [next-ops (assign-map-key-ops ops map-id key-id (key entry) (val entry))]
        (recur next-ops
               (core/next-id next-ops)
               (next entries)))
      ops)))

(defn populate-list-ops
  [ops* list-id the-list start-id]
  (loop [ops       ops*
         insert-id start-id
         after-id  list-id
         items     the-list]
    (if-some [item (first items)]
      (let [next-ops (insert-list-item-ops ops list-id insert-id after-id item)]
        (recur next-ops
               (core/next-id next-ops)
               insert-id
               (next items)))
      ops)))

(defmethod value-to-ops :map
  [ops map-id the-map]
  (populate-map-ops (assoc ops map-id (ops/make-map))
                    map-id
                    the-map
                    (core/successor-id map-id)))

(defmethod value-to-ops :vec
  [ops vector-id the-vector]
  (populate-list-ops (assoc ops vector-id (ops/make-vector))
                     vector-id
                     the-vector
                     (core/successor-id vector-id)))

(defmethod value-to-ops :set
  [ops* set-id the-set]
  (loop [ops   (assoc ops* set-id (ops/make-set))
         items the-set]
    (if-some [item (first items)]
      (let [item-id        (core/next-id ops)
            ops-after-item (value-to-ops ops item-id item)
            assign-id      (core/next-id ops-after-item)]
        (recur (assoc ops-after-item assign-id (ops/assign set-id item-id))
               (next items)))
      ops)))

(defmethod value-to-ops :lst
  [ops list-id the-list]
  (populate-list-ops (assoc ops list-id (ops/make-list))
                     list-id
                     the-list
                     (core/successor-id list-id)))

;;;; Edit to Ops

(defn get-id
  [o]
  (some-> o meta :converge/id))

(defn get-insertion-id
  [o n]
  (some-> o meta :converge/insertions (util/safe-get n)))

(defn get-key-id
  [o k]
  (some-> o meta :converge/keys (get k)))

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
  (let [entity-id     (get-id the-map)
        key-id        (core/next-id ops actor)
        ops-after-key (value-to-ops ops key-id (util/last-indexed path))
        value-id      (core/next-id ops-after-key)
        ops-after-val (value-to-ops ops-after-key value-id v)
        assign-id     (core/next-id ops-after-val)]
    (assoc ops-after-val assign-id (ops/assign entity-id key-id value-id))))

(defmethod -edit-to-ops [:+ :vec]
  [ops actor [path _ item] the-vector]
  (let [entity-id (get-id the-vector)]
    (insert-list-item-ops ops
                          entity-id
                          (core/next-id ops actor)
                          (or (some->> path util/last-indexed dec (get-insertion-id the-vector))
                              entity-id)
                          item)))

(defmethod -edit-to-ops [:+ :set]
  [ops actor [_ _ item] the-set]
  (let [entity-id      (get-id the-set)
        item-id        (core/next-id ops actor)
        ops-after-item (value-to-ops ops item-id item)
        assign-id      (core/next-id ops-after-item)]
    (assoc ops-after-item assign-id (ops/assign entity-id item-id))))

(defmethod -edit-to-ops [:+ :lst]
  [ops actor [path _ item] the-list]
  (let [entity-id (get-id the-list)]
    (insert-list-item-ops ops
                          entity-id
                          (core/next-id ops actor)
                          (or (some->> path util/last-indexed dec (get-insertion-id the-list))
                              entity-id)
                          item)))

;;;; Remove

(defmethod -edit-to-ops [:- :map]
  [ops actor [path] the-map]
  (remove-key-ops ops
                  (get-id the-map)
                  (core/next-id ops actor)
                  (->> path
                       util/last-indexed
                       (get-key-id the-map))))

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
                  (->> path
                       util/last-indexed
                       (get-key-id the-set))))

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
                              (get-key-id old-map k))

              (and (not (contains? old-map k))
                   (contains? new-map k))
              (assign-map-key-ops ops
                                  map-id
                                  (core/next-id ops actor)
                                  k
                                  (get new-map k))

              (and (contains? new-map k)
                   (contains? old-map k)
                   (not= (get old-map k)
                         (get new-map k)))
              (reassign-existing-key-ops ops
                                         map-id
                                         (get-key-id old-map k)
                                         (get new-map k)
                                         (core/next-id ops actor))

              :else ops))
          ops*
          (into #{} (concat (keys old-map) (keys new-map)))))


;; TODO: actually diff here to preserve existing elements
(defn list-diff-ops
  [ops* actor list-id old-list new-list]
  #_(let [old-idx (loop [items old-list
                         i     0
                         idx   (transient {})]
                    (if-let [item (first items)]
                      (recur (next items)
                             (inc i)
                             (assoc! idx
                                     item
                                     {:index        i
                                      :value-id     (get-id item)
                                      :insertion-id (get-insertion-id old-list item)}))
                      (persistent! idx)))])
  (populate-list-ops ops* list-id new-list (core/next-id ops* actor)))

(defn set-diff-ops
  [ops* actor set-id old-set new-set]
  (reduce (fn [ops k]
            (cond
              (and (contains? old-set k)
                   (not (contains? new-set k)))
              (remove-key-ops ops
                              set-id
                              (core/next-id ops actor)
                              (get-key-id old-set k))

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
          (reassign-existing-key-ops ops
                                     (get-id the-list)
                                     (get-insertion-id the-list k)
                                     new-value
                                     (core/next-id ops actor)))
        ;; TODO: attempt to retain inner values when converting from one collection type to another?
        ;; TODO: more broadly, should we cache and re-use scalar (not including tracking text/string) values?
        ;; ... with an entirely new value
        (reassign-existing-key-ops ops
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
          (reassign-existing-key-ops ops
                                     (get-id the-map)
                                     (get-key-id the-map k)
                                     new-value
                                     (core/next-id ops actor)))
        ;; TODO: attempt to retain inner values when converting from one collection type to another?
        ;; TODO: more broadly, should we cache and re-use scalar (not including tracking text/string) values?
        ;; ... with an entirely new value
        (reassign-existing-key-ops ops
                                   (get-id the-map)
                                   (get-key-id the-map k)
                                   new-value
                                   (core/next-id ops actor))))))

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

(defmethod -edit-to-ops [:r :val]
  [ops actor [path _ value] _entity]
  (let [id   (core/next-id ops actor)
        ops* (value-to-ops ops id value)]
    (if (empty? path)
      (if-let [op (get ops* id)]
        (assoc ops* id (assoc-in op [:data :root?] true))
        ops*)
      ops*)))

;; TODO: Use transient log, for performance? (would need to figure out nth on transient AVL map)
(defn edit-to-ops
  [log actor root [path :as edit]]
  (-edit-to-ops log
                actor
                edit
                (util/safe-get-in root (util/safe-pop path))))

(defn make-patch
  [log-orig interpretation actor old-value new-value]
  (loop [edits  (e/get-edits (e/diff old-value new-value))
         value  old-value
         log    log-orig
         interp (or interpretation (interpret/interpret log-orig))]
    (if-let [edit (util/first-indexed edits)]
      (let [next-log    (edit-to-ops log actor value edit)
            next-interp (interpret/interpret
                         interp
                         (avl/subrange next-log > (core/latest-id log)))]
        (recur (subvec edits 1)
               (edn/edn next-interp)
               next-log
               next-interp))
      (let [ops (avl/subrange log > (core/latest-id log-orig))]
        (when-not (empty? ops)
          (some-> log-orig
                  core/ref-root-data-from-log
                  :id
                  (core/->Patch ops)))))))

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

  (def r (converge.api/ref {#{} 0, {} 0} :backend :opset))
  (meta @r)
  (reset! r {})

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
