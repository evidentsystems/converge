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

(defn ensure-key-op
  [{:keys               [log actor]
    {:keys [key-cache]} :interpretation
    :as                 context}
   k]
  (if-let [existing-id (get key-cache k)]
    [context existing-id]
    (let [key-id (domain/next-id log actor)]
      (-> context
          (update-context
           {key-id (ops/make-key k)})
          (vector key-id)))))

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
  [{:keys [log actor]
    :as   context}
   value root?]
  (let [value-id (domain/next-id log actor)]
    (-> context
        (update-context
         {value-id (ops/make-value value root?)})
        (vector value-id))))

(defn add-assign-op
  "Unlike the other add-*-op, this doesn't return a tuple of [context
  op-id], but rather just a bare context."
  ([ctx entity-id attribute-id]
   (add-assign-op ctx entity-id attribute-id nil))
  ([{:keys [log actor] :as context} entity-id attribute-id val-id]
   (let [assign-id (domain/next-id log actor)
         ops {assign-id
              (ops/assign entity-id attribute-id val-id)}]
     (update-context context ops))))

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
            (add-assign-op ctx2 list-id insert-id value-id)]
        (recur ctx3
               insert-id
               (next items)))
      ctx)))

(defmethod -add-value-ops :map
  [{:keys [log actor] :as context} the-map root?]
  (let [map-id (domain/next-id log actor)]
    [(reduce-kv
      (fn [ctx k v]
        (let [[ctx1 key-id]
              (ensure-key-op ctx k)

              [ctx2 value-id]
              (add-value-ops ctx1 v)]
          (add-assign-op ctx2 map-id key-id value-id)))
      (update-context context {map-id (ops/make-map root?)})
      the-map)
     map-id]))

(defmethod -add-value-ops :vec
  [{:keys [log actor] :as context} the-vector root?]
  (let [vector-id (domain/next-id log actor)]
    [(populate-list (update-context
                     context
                     {vector-id (ops/make-vector root?)})
                    vector-id
                    the-vector)
     vector-id]))

(defmethod -add-value-ops :set
  [{:keys [log actor] :as context} the-set root?]
  (let [set-id (domain/next-id log actor)]
    [(reduce
      (fn [ctx k]
        (let [[ctx1 key-id]
              (ensure-key-op ctx k)]
          (add-assign-op ctx1 set-id key-id)))
      (update-context context {set-id (ops/make-set root?)})
      the-set)
     set-id]))

(defmethod -add-value-ops :lst
  [{:keys [log actor] :as context} the-list root?]
  (let [list-id (domain/next-id log actor)]
    [(populate-list (update-context
                     context
                     {list-id (ops/make-list root?)})
                    list-id
                    the-list)
     list-id]))

;;;; Diff existing

(defmulti -diff-existing
  "Returns a tuple of [new-context entity-id]"
  (fn [_context old-value _new-value]
    (domain/get-type old-value))
  :default ::default)

;; TODO: Pour existing values from list/vector into a vector/list if merely
;; changing entity type?
(defn diff-existing
  [context old-value new-value]
  (when (= (domain/get-type old-value)
           (domain/get-type new-value))
    (-diff-existing context old-value new-value)))

(defn ensure-value
  "Returns a tuple of [new-context value-id] by either creating a new
  value in the context, or getting and updating an existing entity."
  ([context value]
   (ensure-value context value false))
  ([context value root?]
   (if-let [existing-id (get-id value)]
     (or (diff-existing context
                        (edn/-edn (:interpretation context)
                                  existing-id)
                        value)
         (add-value-ops context value root?))
     (add-value-ops context value root?))))

(defmethod -diff-existing ::default
  [_ old-value new-value]
  (throw
   (ex-info "Don't know how diff this existing value"
            {:old-value old-value
             :new-value new-value})))

(defmethod -diff-existing :map
  [context old-map new-map]
  (let [map-id    (get-id old-map)
        key-cache (get-in context [:interpretation :keys])]
    [(reduce (fn [ctx k]
               (cond
                 (and (contains? old-map k)
                      (not (contains? new-map k)))
                 (domain/first-indexed
                  (add-remove-op ctx
                                 map-id
                                 (get key-cache k)))

                 (contains? new-map k)
                 (let [[context1 value-id]
                       (ensure-value ctx (get new-map k))

                       [context2 key-id]
                       (ensure-key-op context1 k)]
                   (add-assign-op context2
                                  map-id
                                  key-id
                                  value-id))

                 :else
                 ctx))
             context
             (into #{} (concat (keys old-map) (keys new-map))))
     map-id]))

(defn list-diff
  [context old-value new-value]
  (let [list-id (get-id old-value)

        context1
        (loop [ctx   context
               items old-value
               i     0]
          (if (first items)
            (recur (domain/first-indexed
                    (add-remove-op ctx
                                   list-id
                                   (get-insertion-id old-value i)))
                   (next items)
                   (inc i))
            ctx))]
    (loop [ctx       context1
           after-id  list-id
           items     new-value]
      (if-let [item (first items)]
        (let [[ctx1 value-id]
              (add-value-ops ctx item)

              [ctx2 insert-id]
              (add-insert-op ctx1 after-id)]
          (recur (add-assign-op ctx2 list-id insert-id value-id)
                 insert-id
                 (next items)))
        [ctx list-id]))))

(defmethod -diff-existing :vec
  [context old-value new-value]
  (list-diff context old-value new-value))

(defmethod -diff-existing :set
  [context old-set new-set]
  (let [set-id    (get-id old-set)
        key-cache (get-in context
                          [:interpretation :keys])]
    [(reduce (fn [ctx k]
               (cond
                 (and (contains? old-set k)
                      (not (contains? new-set k)))
                 (domain/first-indexed
                  (add-remove-op ctx
                                 set-id
                                 (get key-cache k)))

                 (contains? new-set k)
                 (let [[context1 key-id]
                       (ensure-key-op ctx k)]
                   (add-assign-op context1
                                  set-id
                                  key-id))

                 :else
                 ctx))
             context
             (into old-set new-set))
     set-id]))

(defmethod -diff-existing :lst
  [context old-value new-value]
  (list-diff context old-value new-value))

;;;; Edit to Ops

(defmulti -edit-to-ops
  "Returns a new context incorporating the given edit."
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
  (let [[context1 value-id]
        (ensure-value context v)

        [context2 key-id]
        (ensure-key-op context1 (domain/last-indexed path))]
    (add-assign-op context2
                   (get-id the-map)
                   key-id
                   value-id)))

(defmethod -edit-to-ops [:+ :vec]
  [context [path _ item] the-vector]
  (let [[context1 value-id]
        (ensure-value context item)

        [context2 insert-id]
        (add-insert-op context1
                       (or (some->> path
                                    domain/last-indexed
                                    dec
                                    (get-insertion-id the-vector))
                           (get-id the-vector)))]
    (add-assign-op context2
                   (get-id the-vector)
                   insert-id
                   value-id)))

(defmethod -edit-to-ops [:+ :set]
  [context [_ _ item] the-set]
  (let [[context1 key-id]
        (ensure-key-op context item)]
    (add-assign-op context1
                   (get-id the-set)
                   key-id)))

(defmethod -edit-to-ops [:+ :lst]
  [context [path _ item] the-list]
  (let [[context1 value-id]
        (ensure-value context item)

        [context2 insert-id]
        (add-insert-op context1
                       (or (some->> path
                                    domain/last-indexed
                                    dec
                                    (get-insertion-id the-list))
                           (get-id the-list)))]
    (add-assign-op context2
                   (get-id the-list)
                   insert-id
                   value-id)))

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

(defmethod -edit-to-ops [:r :map]
  [context [path _ new-value] the-map]
  (if (empty? path)
    (domain/first-indexed
     (ensure-value context new-value true))
    (let [[context1 value-id]
          (ensure-value context new-value)

          [context2 key-id]
          (ensure-key-op context1 (domain/last-indexed path))]
      (add-assign-op context2
                     (get-id the-map)
                     key-id
                     value-id))))

(defmethod -edit-to-ops [:r :vec]
  [context [path _ new-value] the-vector]
  (if (empty? path)
    (domain/first-indexed
     (ensure-value context new-value true))
    (let [[context1 value-id]
          (ensure-value context new-value)]
      (add-assign-op context1
                     (get-id the-vector)
                     (get-insertion-id the-vector
                                       (domain/last-indexed path))
                     value-id))))

(defmethod -edit-to-ops [:r :set]
  [context [path _ new-value] the-set]
  (if (empty? path)
    (domain/first-indexed
     (ensure-value context new-value true))
    (let [[context1 key-id]
          (ensure-key-op context (domain/last-indexed path))]
      (add-assign-op context1
                     (get-id the-set)
                     key-id))))

(defmethod -edit-to-ops [:r :lst]
  [context [path _ new-value] the-list]
  (if (empty? path)
    (domain/first-indexed
     (ensure-value context new-value true))
    (let [[context1 value-id]
          (ensure-value context new-value)]
      (add-assign-op context1
                     (get-id the-list)
                     (get-insertion-id the-list
                                       (domain/last-indexed path))
                     value-id))))

(defmethod -edit-to-ops [:r :val]
  [context [path _ value] _entity]
  (domain/first-indexed
   (add-value-ops context value (empty? path))))

(defn recompute-root
  [{:keys [interpretation] :as context}]
  (assoc context :root (edn/edn interpretation)))

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
    (if (= (:root context) new-value)
      (when-not (empty? ops)
        (domain/map->Patch
         {:source         (-> log-orig
                              domain/ref-root-data-from-log
                              :id)
          :ops            ops
          :interpretation (:interpretation context)
          :value          (:root context)}))
      (throw (ex-info "Invalid patch: new-value computed from patch doesn't match the provided new-value."
                      {:ops                 ops
                       :old-value           old-value
                       :new-value           new-value
                       :new-value-for-patch (:root context)})))))

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
