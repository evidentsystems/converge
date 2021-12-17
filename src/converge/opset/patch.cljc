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
            [converge.domain :as domain]
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
  (some-> o meta :converge/insertions (domain/safe-get n)))

(defn update-context
  [context ops]
  (-> context
      (update :log merge ops)
      (update :interpretation interpret/interpret ops)))

;;;; Value to Ops

(defn add-key-op
  [{:keys               [log actor]
    {:keys [key-cache]} :interpretation
    :as                 context}
   k]
  (let [key-id (or (get key-cache k)
                   (domain/next-id log actor))
        ops      {key-id (ops/make-key k)}]
    (-> context
        (update-context ops)
        (vector key-id))))

(defmulti -add-value-ops
  (fn [_context value _root?]
    (domain/get-type value))
  :default ::default)

(defn add-value-ops
  ([context value]
   (add-value-ops context value false))
  ([context value root?]
   (-add-value-ops context value root?)))

(defmethod -add-value-ops ::default
  [_ value root?]
  (throw
   (ex-info "Don't know how to add this value to the opset"
            {:value value
             :root? root?})))

(defn add-assign-op
  ([ctx entity-id attribute-id]
   (add-assign-op ctx entity-id attribute-id nil))
  ([{:keys [log actor] :as context} entity-id attribute-id val-id]
   (let [assign-id (domain/next-id log actor)
         ops {assign-id
              (ops/assign entity-id attribute-id val-id)}]
     (-> context
         (update-context ops)
         (vector assign-id)))))

(defn add-remove-op
  [{:keys [log actor] :as context} entity-id attribute-id]
  (let [remove-id (domain/next-id log actor)
        ops {remove-id
             (ops/remove entity-id attribute-id)}]
    (-> context
        (update-context ops)
        (vector remove-id))))

(defn add-insert-op
  [{:keys [log actor] :as context} after-id]
  (let [insert-id (domain/next-id log actor)
        ops       {insert-id (ops/insert after-id)}]
    (-> context
        (update-context ops)
        (vector insert-id))))

(defn populate-list
  [ctx* list-id the-list]
  (loop [ctx       ctx*
         after-id  list-id
         items     the-list]
    (if-some [item (first items)]
      (let [[ctx1 value-id]
            (add-value-ops ctx item)

            [ctx2 insert-id]
            (add-insert-op ctx1 after-id)

            ctx3
            (domain/first-indexed
             (add-assign-op ctx2 list-id insert-id value-id))]
        (recur ctx3
               insert-id
               (next items)))
      ctx)))

(defmethod -add-value-ops :val
  [{:keys                 [log actor]
    {:keys [value-cache]} :interpretation
    :as                   context}
   value root?]
  (let [value-id (or (get value-cache value)
                     (domain/next-id log actor))
        ops      {value-id
                  (ops/make-value value root?)}]
    (-> context
        (update-context ops)
        (vector value-id))))

(defmethod -add-value-ops :map
  [{:keys [log actor] :as context} the-map root?]
  (let [map-id (domain/next-id log actor)
        op     (ops/make-map root?)
        ops    {map-id op}]
    [(reduce-kv
      (fn [ctx k v]
        (let [[ctx1 key-id]
              (add-key-op ctx k)
              [ctx2 value-id]
              (add-value-ops ctx1 v)]
          (domain/first-indexed
           (add-assign-op ctx2 map-id key-id value-id))))
      (update-context context ops)
      the-map)
     map-id]))

(defmethod -add-value-ops :vec
  [{:keys [log actor] :as context} the-vector root?]
  (let [vector-id (domain/next-id log actor)
        op        (ops/make-vector root?)
        ops       {vector-id op}]
    [(populate-list (update-context context ops)
                    vector-id
                    the-vector)
     vector-id]))

(defmethod -add-value-ops :set
  [{:keys [log actor] :as context} the-set root?]
  (let [set-id (domain/next-id log actor)
        op     (ops/make-set root?)
        ops    {set-id op}]
    [(reduce
      (fn [ctx k]
        (let [[ctx1 key-id]
              (add-key-op ctx k)]
          (domain/first-indexed
           (add-assign-op ctx1 set-id key-id))))
      (update-context context ops)
      the-set)
     set-id]))

(defmethod -add-value-ops :lst
  [{:keys [log actor] :as context} the-list root?]
  (let [list-id (domain/next-id log actor)
        op      (ops/make-list root?)
        ops     {list-id op}]
    [(populate-list (update-context context ops)
                    list-id
                    the-list)
     list-id]))

;;;; Edit to Ops

(defmulti -edit-to-ops
  "Returns a vector of tuples of [id op] that represent the given Editscript edit."
  (fn [_context edit entity]
    [(nth edit 1)
     (domain/get-type entity)]))

(defmethod -edit-to-ops :default
  [_context edit entity]
  (throw
   (ex-info "Unknown edit operation"
            {:edit   edit
             :entity entity
             :op     (nth edit 1)
             :type   (domain/get-type entity)})))

;;;; Add

(defmethod -edit-to-ops [:+ :map]
  [context [path _ v] the-map]
  (let [[value-id op]
        (if-let [existing-id (get-id v)]
          [existing-id nil]
          [])]
    context))

(defmethod -edit-to-ops [:+ :vec]
  [context [path _ item] the-vector]
  context)

(defmethod -edit-to-ops [:+ :set]
  [context [_ _ item] the-set]
  context)

(defmethod -edit-to-ops [:+ :lst]
  [context [path _ item] the-list]
  context)

;;;; Remove

(defmethod -edit-to-ops [:- :map]
  [{{:keys [key-cache]} :interpretation
    :as context}
   [path] the-map]
  (domain/first-indexed
   (add-remove-op context
                  (get-id the-map)
                  (get key-cache
                       (domain/last-indexed path)))))

(defmethod -edit-to-ops [:- :vec]
  [context [path] the-vector]
  (domain/first-indexed
   (add-remove-op context
                  (get-id the-vector)
                  (get-insertion-id
                   the-vector
                   (domain/last-indexed path)))))

(defmethod -edit-to-ops [:- :set]
  [{{:keys [key-cache]} :interpretation
    :as context}
   [path] the-set]
  (domain/first-indexed
   (add-remove-op context
                  (get-id the-set)
                  (get key-cache
                       (domain/last-indexed path)))))

(defmethod -edit-to-ops [:- :lst]
  [context [path] the-list]
  (domain/first-indexed
   (add-remove-op context
                  (get-id the-list)
                  (get-insertion-id
                   the-list
                   (domain/last-indexed path)))))

;;;; Replace

#_(defn map-diff-ops
  [ops* actor map-id old-map new-map]
  (reduce (fn [ops k]
            (cond
              (and (contains? old-map k)
                   (not (contains? new-map k)))
              (remove-key-ops ops
                              map-id
                              (domain/next-id ops actor)
                              k)

              (and (not (contains? old-map k))
                   (contains? new-map k))
              (let [value         (get new-map k)
                    key-id        (domain/next-id ops actor)
                    ops-after-key (assoc ops key-id (ops/make-key k))]
                (if-let [val-id (get-id value)]
                  (assign-key-ops
                   ops-after-key
                   map-id
                   (domain/successor-id key-id)
                   key-id
                   val-id)
                  (create-value-and-assign-key-ops
                   ops-after-key
                   map-id
                   key-id
                   value
                   (domain/successor-id key-id))))

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
#_(defn list-diff-ops
  [ops* actor list-id old-list new-list]
  (let [ops-after-removals
        (loop [ops ops*
               items old-list
               i    0]
          (if (first items)
            (recur (remove-key-ops ops
                                   list-id
                                   (domain/next-id ops actor)
                                   (get-insertion-id old-list i))
                   (next items)
                   (inc i))
            ops))]
    (loop [ops       ops-after-removals
           insert-id (domain/next-id ops-after-removals actor)
           after-id  list-id
           items     new-list]
      (if-let [item (first items)]
        (let [next-ops (if-let [id (get-id item)]
                         (insert-list-item-ops
                          ops list-id insert-id after-id id)
                         (create-and-insert-list-item-ops
                          ops list-id insert-id after-id item))]
          (recur next-ops
                 (domain/next-id next-ops)
                 insert-id
                 (next new-list)))
        ops))))

#_(defn set-diff-ops
  [ops* actor set-id old-set new-set]
  (reduce (fn [ops k]
            (cond
              (and (contains? old-set k)
                   (not (contains? new-set k)))
              (remove-key-ops ops
                              set-id
                              (domain/next-id ops actor)
                              k)

              (and (not (contains? old-set k))
                   (contains? new-set k))
              (assign-set-member-ops ops
                                     set-id
                                     (domain/next-id ops actor)
                                     k)

              :else ops))
          ops*
          (into old-set new-set)))

#_(defn replace-in-list-ops
  [ops actor path new-value the-list]
  (if (empty? path)
    ;; Replacing the root list...
    (if (= (domain/get-type new-value) :list)
      ;; ...with minimal ops while retaining list identity, since new value is also a list
      (list-diff-ops ops actor (get-id the-list) the-list new-value)
      ;; ...entirely with a new value with :root? true
      (let [value-id (domain/next-id ops actor)
            ops*     (value-to-ops ops value-id new-value)]
        (assoc ops* value-id (assoc-in (get ops* value-id) [:data :root?] true))))
    ;; Replacing a key within this list...
    (let [k         (domain/last-indexed path)
          old-value (domain/safe-get the-list k)]
      (if (= (domain/get-type old-value)
             (domain/get-type new-value))
        ;; ...with minimal ops while attempting to retain value
        ;; identity, since new value is same type
        (case (domain/get-type old-value)
          :map (map-diff-ops ops actor (get-id old-value) old-value new-value)
          (:vec :lst) (list-diff-ops ops actor (get-id old-value) old-value new-value)
          :set (set-diff-ops ops actor (get-id old-value) old-value new-value)
          ;; else replace a primitive type
          (create-value-and-assign-key-ops ops
                                           (get-id the-list)
                                           (get-insertion-id the-list k)
                                           new-value
                                           (domain/next-id ops actor)))
        ;; TODO: attempt to retain inner values when converting from one collection type to another?
        ;; TODO: more broadly, should we cache and re-use scalar (not including tracking text/string) values?
        ;; ... with an entirely new value
        (create-value-and-assign-key-ops ops
                                         (get-id the-list)
                                         (get-insertion-id the-list k)
                                         new-value
                                         (domain/next-id ops actor))))))

(defmethod -edit-to-ops [:r :map]
  [context [path _ new-value] the-map]
  context
  #_(if (empty? path)
      ;; Replacing the root map...
      (if (= (domain/get-type new-value) :map)
        ;; ...with minimal ops while retaining map identity, since new value is also a map
        (map-diff-ops ops actor (get-id the-map) the-map new-value)
        ;; ...entirely with a new value with :root? true
        (let [value-id (domain/next-id ops actor)
              ops*     (value-to-ops ops value-id new-value)]
          (assoc ops* value-id (assoc-in (get ops* value-id) [:data :root?] true))))
      ;; Replacing a key within this map...
      (let [k         (domain/last-indexed path)
            old-value (get the-map k)]
        (if (= (domain/get-type old-value)
               (domain/get-type new-value))
          ;; ...with minimal ops while attempting to retain value
          ;; identity, since new value is same type
          (case (domain/get-type old-value)
            :map (map-diff-ops ops actor (get-id old-value) old-value new-value)
            (:vec :lst) (list-diff-ops ops actor (get-id old-value) old-value new-value)
            :set (set-diff-ops ops actor (get-id old-value) old-value new-value)
            ;; else replace a primitive type
            (let [key-id (domain/next-id ops actor)]
              (create-value-and-assign-key-ops (assoc ops key-id (ops/make-key k))
                                               (get-id the-map)
                                               key-id
                                               new-value
                                               (domain/successor-id key-id))))
          ;; TODO: attempt to retain inner values when converting from one collection type to another?
          ;; TODO: more broadly, should we cache and re-use scalar (not including tracking text/string) values?
          ;; ... with an entirely new value
          (let [key-id (domain/next-id ops actor)]
            (create-value-and-assign-key-ops (assoc ops key-id (ops/make-key k))
                                             (get-id the-map)
                                             key-id
                                             new-value
                                             (domain/successor-id key-id)))))))

(defmethod -edit-to-ops [:r :vec]
  [context [path _ new-value] the-vector]
  context
  #_(replace-in-list-ops ops actor path value the-vector))

(defmethod -edit-to-ops [:r :set]
  [context [path _ new-value] the-set]
  context
  #_(if (empty? path)
      ;; Replacing the root set...
      (if (= (domain/get-type new-value) :set)
        ;; ...with minimal ops while retaining set identity, since new value is also a set
        (set-diff-ops ops actor (get-id the-set) the-set new-value)
        ;; ...entirely with a new value with :root? true
        (let [value-id (domain/next-id ops actor)
              ops*     (value-to-ops ops value-id new-value)]
          (assoc ops* value-id (assoc-in (get ops* value-id) [:data :root?] true))))
      ;; We don't replace keys within set...
      ops))

(defmethod -edit-to-ops [:r :lst]
  [context [path _ value] the-list]
  context
  #_(replace-in-list-ops ops actor path value the-list))

(defmethod -edit-to-ops [:r :val]
  [context [path _ value] _entity]
  (domain/first-indexed (add-value-ops context value (empty? path))))

(defn recompute-root
  [{:keys [interpretation] :as context}]
  (assoc context :value (edn/edn interpretation)))

(defn edit-to-ops
  [{:keys [root] :as context} [path :as edit]]
  (-> context
      (-edit-to-ops edit
                    (domain/safe-get-in root (domain/safe-pop path)))
      recompute-root))

(defn make-patch
  [log-orig interpretation actor old-value new-value]
  (let [context (reduce edit-to-ops
                        {:actor actor
                         :log   log-orig
                         :root  old-value
                         :interpretation
                         (or interpretation (interpret/interpret log-orig))}
                        (e/get-edits
                         (e/diff old-value new-value)))
        ops     (avl/subrange (:log context) > (domain/latest-id log-orig))]
    (if (= (:value context) new-value)
      (when-not (empty? ops)
        (domain/map->Patch
         {:source         (-> log-orig
                              domain/ref-root-data-from-log
                              :id)
          :ops            ops
          :interpretation (:interpretation context)
          :value          (:value context)}))
      (throw (ex-info "Invalid patch: new-value computed from patch doesn't match the provided new-value."
                      {:ops                          ops
                       :old-value                    old-value
                       :new-value                    new-value
                       :new-value-computed-for-patch (:value context)})))))

(comment

  (def ref-id
    (domain/uuid))

  (def creator
    (domain/uuid))

  (def log1
    (domain/make-log
     (domain/make-id)
     (domain/root-op ref-id creator :opset)))

  (edn/edn (interpret/interpret log1))

  (def actor (domain/uuid))

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

  (def r (converge.api/ref nil :backend :opset))
  @r
  (reset! r :foo)
  (reset! r [:foo :bar :baz :quux 1 2 3])
  (swap! r pop)

  (e/diff @r (pop @r))

  (meta @r)

  (converge.api/ref-log r)

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

  (e/diff {:foo :bar :baz :quux} {:foo :bar})

  ;; end
  )
