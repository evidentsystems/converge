(ns converge.api
  "The public API of the Converge library, exposing a convergent
  reference type that acts like a Clojure Atom, with additional
  functions for comparing, merging, and patching these convergent
  refs."
  (:refer-clojure :exclude [ref])
  (:require [converge.opset :as opset]
            [converge.ref :as ref]
            [converge.util :as util]))

#?(:clj (set! *warn-on-reflection* true))

;; TODO: add note to docstring about our special top-level type
;; validation logic
(defn ref
  "Creates and returns a ConvergentRef with an initial value of `x` and
  zero or more options (in any order):

  :actor a UUID

  :meta metadata-map

  :validator validate-fn

  The actor uniquely identifies the origin of all changes made on this
  convergent ref, and must be globally unique for all sites/users of the
  underlying CRDT. If not provided, a random actor UUID will be
  generated. If metadata-map is supplied, it will become the metadata
  on the atom. validate-fn must be nil or a side-effect-free fn of one
  argument, which will be passed the intended new state on any state
  change. If the new state is unacceptable, the validate-fn should
  return false or throw an Error.  If either of these error conditions
  occur, then the value of the atom will not change."
  [initial-value & {:keys [actor meta validator] :as options}]
  (assert (or (nil? actor) (uuid? actor))
          "Option `:actor`, if provided, must be a UUID")
  (let [validator* (or validator (constantly true))
        actor*     (or actor (util/uuid))

        r
        (cond
          (map? initial-value)
          (ref/->ConvergentRef actor*
                               (ref/->ConvergentState (opset/opset opset/root-id (opset/make-map)) nil false)
                               meta
                               #(and (map? %) (validator* %))
                               nil)

          (vector? initial-value)
          (ref/->ConvergentRef actor*
                               (ref/->ConvergentState (opset/opset opset/root-id (opset/make-list)) nil false)
                               meta
                               #(and (vector? %) (validator* %))
                               nil)

          :else
          (throw (ex-info "The initial value of a convergent ref must be either a map or a vector."
                          {:initial-value initial-value})))]
    (reset! r initial-value)
    r))

(defn ref-from-opset
  "Creates and returns a ConvergentRef from the given `opset` and zero
  or more options (in any order):

  :actor a UUID

  :meta metadata-map

  :validator validate-fn

  The actor uniquely identifies the origin of all changes made on this
  convergent ref, and must be globally unique for all sites/users of
  the underlying CRDT. If not provided, a random actor UUID will be
  generated. If metadata-map is supplied, it will become the metadata
  on the atom. validate-fn must be nil or a side-effect-free fn of one
  argument, which will be passed the intended new state on any state
  change. If the new state is unacceptable, the validate-fn should
  return false or throw an Error.  If either of these error conditions
  occur, then the value of the atom will not change."
  [opset & {:keys [actor meta validator] :as options}]
  (assert (or (nil? actor) (uuid? actor))
          "Option `:actor`, if provided, must be a UUID")
  ;; TODO: assertions ensuring valid opset
  (let [opset*         (into (opset/opset) opset)
        validator*     (or validator (constantly true))
        actor*         (or actor (util/uuid))
        initial-action (get-in opset* [opset/root-id :action])
        type-pred      (case initial-action
                         :make-map  map?
                         :make-list vector?
                         (throw (ex-info "Invalid opset" {:opset opset*})))
        r              (ref/->ConvergentRef actor*
                                            (ref/->ConvergentState opset* nil true)
                                            meta
                                            #(and (type-pred %) (validator* %))
                                            nil)]
    @r
    r))

(defn opset
  [cr]
  (some-> cr ref/-state :opset))

(defn patch!
  [cr ops]
  )

;; TODO: ensure opset isn't just patch (i.e. an opset? predicate
;; asserting that first op has root-id -> make-map/list)
;; TODO: ensure actor id is different prior to merge?
(defn merge!
  [cr opset]
  )

(comment

  (require '[criterium.core :as criterium])

  (def m (ref {}))
  @m
  (ref/-state m)

  (def v (ref []))
  @v
  (ref/-state v)

  (def a
    {:empty-m {}
     :empty-l []
     :a       :key
     :another {:nested {:key [1 2 3]}}
     :a-list  [:foo "bar" 'baz {:nested :inalist}]
     :a-set   #{1 2 3 4 5}})

  (criterium/bench
   (ref a))

  (simple-benchmark [m a f ref] (f a) 1000)

  (def c (ref a))
  @c
  (opset c)

  (simple-benchmark [c (ref a) v (random-uuid)] (swap! c assoc :foo v) 1000)

  (def b
    {:empty-m {}
     :empty-l []
     :a       :nother
     :another {:nested {:key    [1 2 3]
                        :deeply :mcnested}}
     :a-list  [:foo "bar" {:nested :inalist}]
     :a-set   #{1 3 4 5}})

  (def c (ref {}))
  (reset! c b)
  (reset! c {})

  (swap! c assoc :foo 'bar :baz "quux")
  (swap! c dissoc :foo)

  (binding [*print-meta* true]
    (prn @c))
  (ref/-state c)

  (criterium/bench
   (swap! c dissoc :a))
  @c

  )
