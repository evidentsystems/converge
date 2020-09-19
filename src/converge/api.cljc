(ns converge.api
  "The public API of the Converge library, exposing a `convergent-ref`
  reference type that acts like a Clojure Atom, with additional
  functions for comparing, merging, and patching these convergent
  refs."
  (:require [converge.opset :as opset]
            [converge.ref :as ref]
            [converge.util :as util]))

(defn convergent-ref
  "Creates and returns a ConvergentRef with an initial value of `x` and
  zero or more options (in any order):

  :actor a UUID

  :meta metadata-map

  :validator validate-fn

  The actor uniquely identifies the origin of all changes made on this
  convergent-ref, and must be globally unique for all sites/users of
  the underlying CRDT. If not provided, a random actor UUID will be
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

        r
        (cond
          (map? initial-value)
          (ref/->ConvergentRef (or actor (util/uuid))
                               (ref/->ConvergentState (opset/opset opset/root-id (opset/make-map)) nil false)
                               meta
                               #(and (map? %) (validator* %))
                               nil)

          (vector? initial-value)
          (ref/->ConvergentRef (or actor (util/uuid))
                               (ref/->ConvergentState (opset/opset opset/root-id (opset/make-list)) nil false)
                               meta
                               #(and (vector? %) (validator* %))
                               nil)

          :else
          (throw (ex-info "The initial value of a convergent-ref must be either a map or a vector."
                          {:initial-value initial-value})))]
    (if initial-value
      (reset! r initial-value))
    r))

(comment

  (def m (convergent-ref {}))
  @m
  (ref/-state m)

  (def v (convergent-ref []))
  @v
  (ref/-state v)

  (def a (convergent-ref
          {:empty-m {}
           :empty-l []
           :a       :key
           :another {:nested {:key [1 2 3]}}
           :a-list  [:foo "bar" 'baz {:nested :inalist}]
           :a-set   #{1 2 3 4 5}}))
  (binding [*print-meta* true]
    (prn @a))
  (ref/-state a)

  (swap! a dissoc :a)
  @a

  )
