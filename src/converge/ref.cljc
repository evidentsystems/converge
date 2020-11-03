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
(ns converge.ref
  "Datatypes and functions implementing a serializable, Atom-like
  convergent reference type."
  (:require [converge.opset :as opset]
            [converge.edn :as edn])
  #?(:clj (:import [clojure.lang IAtom IReference IRef])))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(defrecord ConvergentState [opset value ^boolean dirty?])

;; TODO: patch caches?
(defrecord Patch [ops])

(defn patch?
  [o]
  (instance? Patch o))

(defn notify-w
  [this watches old-value new-value]
  (doseq [[k w] watches]
    (if w (w k this old-value new-value))))

;; TODO: is it necessary to maintain consistent top-level type?
(defn valid?
  [validator old-value new-value]
  (let [type-pred (if (or (map? old-value)
                          (and (nil? old-value)
                               (map? new-value)))
                    map?
                    sequential?)]
    (and (type-pred new-value)
         (if (ifn? validator) (validator new-value) true))))

;; TODO trim down to bare semantics, split other ops into different protocols
(defprotocol IConvergent
  (-actor [this]
    "Returns the current actor of this convergent")
  (-state [this]
    "Returns the current state of this convergent")
  (-set-actor! [this actor]
    "Sets this ref's actor to the given value")
  (-opset [this]
    "Returns this convergent's opset")
  (-make-patch [this new-value]
    "Returns a Patch representing the changes necessary to reset from
    the current value to the provided value.")
  (-state-from-patch [this patch]
    "Returns a new ConvergentState that is the result of applying the
    given patch to the current state.")
  (-apply-state! [this state]
    "Sets this ref's state to the given value and notifies watches. USE WITH CAUTION!")
  (-peek-patches [this]
    "Returns the next patch from this ref's queue of applied patches.")
  (-pop-patches! [this]
    "Mutates this ref's queue of applied patches as with pop, and
    returns the queue's new value."))

#?(:clj
   (deftype ConvergentRef [^:volatile-mutable actor
                           ^:volatile-mutable state
                           ^:volatile-mutable patches
                           ^:volatile-mutable meta
                           ^:volatile-mutable validator
                           ^:volatile-mutable watches]
     IConvergent
     (-actor [this] actor)
     (-state [this] state)
     (-set-actor! [this new-actor] (set! actor new-actor))
     (-opset [this] (:opset state))
     (-apply-state! [this new-state]
       (let [old-value (:value state)]
         (set! state new-state)
         (notify-w this watches old-value (:value new-state))))
     (-make-patch
       [this new-value]
       (assert (valid? validator (:value state) new-value) "Validator rejected reference state")
       (let [{:keys [value opset] :as s} state]
         (some->> new-value (opset/ops-from-diff opset actor value) ->Patch)))
     (-state-from-patch [this patch]
       (if (patch? patch)
         (let [{:keys [ops]}               patch
               {:keys [value opset] :as s} state
               new-opset                   (into opset ops)]
           (assoc s
                  :value (edn/edn new-opset)
                  :dirty? false
                  :opset new-opset))
         state))
     (-peek-patches [this] (peek patches))
     (-pop-patches! [this] (set! patches (pop patches)))

     IAtom
     (reset
       [this new-value]
       (let [patch     (-make-patch this new-value)
             new-state (-state-from-patch this patch)]
         (assert (= new-value (:value new-state)) "Unsupported reference state")
         (-apply-state! this new-state)
         (when patch (set! patches (conj patches patch)))
         (:value new-state)))
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
       (assert (valid? f (:value state) (:value state)) "Validator rejected reference state")
       (set! validator f))
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
                           ^:mutable patches
                           meta
                           validator
                           ^:mutable watches]
     IConvergent
     (-actor [this] actor)
     (-state [this] state)
     (-set-actor! [this new-actor] (set! actor new-actor))
     (-opset [this] (:opset state))
     (-apply-state! [this new-state]
       (let [old-value (:value state)]
         (set! state new-state)
         (notify-w this watches old-value (:value new-state))))
     (-make-patch
       [this new-value]
       (assert (valid? validator (:value state) new-value) "Validator rejected reference state")
       (let [{:keys [value opset] :as s} state]
         (some->> new-value (opset/ops-from-diff opset actor value) ->Patch)))
     (-state-from-patch [this patch]
       (if (patch? patch)
         (let [{:keys [ops]}               patch
               {:keys [value opset] :as s} state
               new-opset                   (into opset ops)]
           (assoc s
                  :value (edn/edn new-opset)
                  :dirty? false
                  :opset new-opset))
         state))
     (-peek-patches [this] (peek patches))
     (-pop-patches! [this] (set! patches (pop patches)))

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
       (let [patch     (-make-patch this new-value)
             new-state (-state-from-patch this patch)]
         (assert (= new-value (:value new-state)) "Unsupported reference state")
         (-apply-state! this new-state)
         (when patch (set! patches (conj patches patch)))
         (:value new-state)))

     ISwap
     (-swap! [this f]          (-reset! this (f (:value state))))
     (-swap! [this f a]        (-reset! this (f (:value state) a)))
     (-swap! [this f a b]      (-reset! this (f (:value state) a b)))
     (-swap! [this f a b args] (-reset! this (apply f (:value state) a b args)))

     IWithMeta
     (-with-meta [_ new-meta] (ConvergentRef. actor state patches new-meta validator watches))

     IMeta
     (-meta [_] meta)

     IPrintWithWriter
     (-pr-writer [this writer opts]
       (-write writer "#object[converge.ref.ConvergentRef ")
       (pr-writer {:val (-deref this)} writer opts)
       (-write writer "]"))

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
