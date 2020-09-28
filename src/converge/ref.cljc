(ns converge.ref
  "Datatypes and functions implementing a serializable, Atom-like
  convergent reference type."
  (:require [converge.opset :as opset]
            [converge.edn :as edn])
  #?(:clj (:import [clojure.lang IAtom IReference IRef])))

#?(:clj (set! *warn-on-reflection* true))

(defrecord ConvergentState [opset value ^boolean dirty?])

(defn notify-w
  [this watches old-value new-value]
  (doseq [[k w] watches]
    (if w (w k this old-value new-value))))

(defprotocol IConvergentRef
  (-set-actor! [this actor] "Sets this ref's actor to the given value")
  (-state [this] "Returns the current state of the convergent ref")
  (-update-state! [this ops] "Adds the ops to the opset, and recompute the value setting dirty? to false"))

#?(:clj
   (deftype ConvergentRef [^:volatile-mutable actor
                           ^:volatile-mutable state
                           ^:volatile-mutable meta
                           ^:volatile-mutable validator
                           ^:volatile-mutable watches]
     IConvergentRef
     (-set-actor! [this new-actor] (set! actor new-actor))
     (-state [this] state)

     IAtom
     (reset
       [this new-value]
       (assert (validator new-value) "Validator rejected reference state")
       (let [{:keys [value opset] :as s} state
             new-opset                   (opset/add-ops-from-diff opset actor value new-value)
             computed-new-value          (edn/edn new-opset)]
         #_(prn {:new-value          new-value
                 :computed-new-value computed-new-value
                 :opset              new-opset})
         (assert (= new-value computed-new-value) "Unsupported reference state")
         (set! state (assoc s
                            :value computed-new-value
                            :dirty? false
                            :opset new-opset))
         (notify-w this watches value new-value)
         new-value))
     (swap [this f]          (.reset this (f (:value state))))
     (swap [this f a]        (.reset this (f (:value state) a)))
     (swap [this f a b]      (.reset this (f (:value state) a b)))
     (swap [this f a b args] (.reset this (apply f (:value state) a b args)))
     (compareAndSet
       [this old-value new-value]
       (if (= (.deref this) old-value)
         (do (.reset this new-value) true)
         false))

     IReference
     (meta [_] meta)
     (alterMeta [this f args] (.resetMeta this (apply f meta args)))
     (resetMeta [_ new-meta]  (set! meta new-meta))

     IRef
     (deref
       [this]
       (let [{:keys [dirty? value opset] :as s}
             state]
         (if dirty?
           (let [value (edn/edn opset)]
             (set! state
                   (assoc s
                          :value  value
                          :dirty? false))
             value)
           value)))
     (setValidator
       [_ f]
       (let [value      (:value state)
             validator* (if (map? value)
                          #(and (map? %)    (f %))
                          #(and (vector? %) (f %)))]
         (assert (validator* value) "Validator rejected reference state")
         (set! validator validator*)))
     (getValidator [_] validator)
     (getWatches   [_] watches)
     (addWatch
       [this k callback]
       (set! watches (assoc watches k callback))
       this)
     (removeWatch
       [this k]
       (set! watches (dissoc watches k))
       this))

   :cljs
   (deftype ConvergentRef [^:mutable actor
                           ^:mutable state
                           meta
                           validator
                           ^:mutable watches]
     IConvergentRef
     (-set-actor! [this new-actor] (set! actor new-actor))
     (-state [this] state)

     IAtom

     IDeref
     (-deref
       [this]
       (let [{:keys [dirty? value opset] :as s}
             state]
         (if dirty?
           (let [value (edn/edn opset)]
             (set! state
                   (assoc s
                          :value  value
                          :dirty? false))
             value)
           value)))

     IEquiv
     (-equiv [this other] (identical? this other))

     IReset
     (-reset!
       [this new-value]
       (assert (validator new-value) "Validator rejected reference state")
       (let [{:keys [value opset] :as s} state
             new-opset                   (opset/add-ops-from-diff opset actor value new-value)
             computed-new-value          (edn/edn new-opset)]
         #_(prn {:new-value          new-value
                 :computed-new-value computed-new-value
                 :opset              new-opset})
         (assert (= new-value computed-new-value) "Unsupported reference state")
         (set! state (assoc s
                            :value computed-new-value
                            :dirty? false
                            :opset new-opset))
         (notify-w this watches value new-value)
         new-value))

     ISwap
     (-swap! [this f]          (-reset! this (f (:value state))))
     (-swap! [this f a]        (-reset! this (f (:value state) a)))
     (-swap! [this f a b]      (-reset! this (f (:value state) a b)))
     (-swap! [this f a b args] (-reset! this (apply f (:value state) a b args)))

     IWithMeta
     (-with-meta [_ new-meta] (ConvergentRef. actor state new-meta validator watches))

     IMeta
     (-meta [_] meta)

     ;; TODO: printing
     #_IPrintWithWriter
     #_(-pr-writer [this w opts] (pr-atom this w opts "ConvergentRef" {:val (-deref a)}))

     IWatchable
     (-notify-watches
       [this old-value new-value]
       (notify-w this watches old-value new-value))
     (-add-watch
       [this k callback]
       (set! watches (assoc watches k callback))
       this)
     (-remove-watch
       [this k]
       (set! watches (dissoc watches k))
       this)

     IHash
     (-hash [this] (goog/getUid this))))
