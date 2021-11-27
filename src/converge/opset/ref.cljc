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
(ns converge.opset.ref
  (:require [converge.core :as core]
            [converge.util :as util]
            [converge.opset.ops :as ops]
            [converge.opset.edn :as edn]
            [converge.opset.interpret :as interpret]
            [converge.opset.patch :as patch])
  #?(:clj (:import [clojure.lang IAtom IReference IRef])))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

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

(defrecord OpsetConvergentState [opset interpretation value ^boolean dirty?])

(deftype OpsetConvergentRef #?(:clj  [^:volatile-mutable actor
                                      ^:volatile-mutable state
                                      ^:volatile-mutable patches
                                      ^:volatile-mutable meta
                                      ^:volatile-mutable validator
                                      ^:volatile-mutable watches]
                               :cljs [^:mutable actor
                                      ^:mutable state
                                      ^:mutable patches
                                      meta
                                      validator
                                      ^:mutable watches])

  core/ConvergentRef
  (-actor [_] actor)
  (-state [_] state)
  (-set-actor! [_ new-actor] (set! actor new-actor))
  (-opset [_] (:opset state))
  (-apply-state! [this new-state]
    (let [old-value (:value state)]
      (set! state new-state)
      (core/notify-w this watches old-value (:value new-state))))
  (-make-patch
    [_ new-value]
    (assert (valid? validator (:value state) new-value) "Validator rejected reference state")
    (let [{:keys [value opset interpretation]} state]
      (patch/make-patch opset interpretation actor value new-value)))
  (-state-from-patch [_ patch]
    (if (core/patch? patch)
      (let [{:keys [ops]}
            patch

            {:keys [interpretation opset]}
            state

            new-opset
            (into opset ops)

            new-interpretation
            (if interpretation
              (interpret/interpret interpretation ops)
              (interpret/interpret new-opset))]
        (->OpsetConvergentState new-opset
                                new-interpretation
                                (edn/edn new-interpretation)
                                false))
      state))
  (-peek-patches [_] (peek patches))
  (-pop-patches! [_] (set! patches (pop patches)))

  #?@(:clj
      [IAtom
       (reset
        [this new-value]
        (let [patch     (core/-make-patch this new-value)
              new-state (core/-state-from-patch this patch)]
          (core/validate-reset (:value state) new-value new-state patch)
          (when patch (set! patches (conj patches patch)))
          (core/-apply-state! this new-state)
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
        [_]
        (let [{:keys [opset interpretation value dirty?] :as s}
              state]
          (if dirty?
            (let [new-interpretation
                  (or interpretation
                      (interpret/interpret opset))

                  value
                  (edn/edn new-interpretation)]
              (set! state
                    (assoc s
                           :interpretation new-interpretation
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
        this)]

      :cljs
      [IAtom

       IDeref
       (-deref
        [_]
        (let [{:keys [opset interpretation value dirty?] :as s}
              state]
          (if dirty?
            (let [new-interpretation
                  (or interpretation
                      (interpret/interpret opset))

                  value
                  (edn/edn new-interpretation)]
              (set! state
                    (assoc s
                           :interpretation new-interpretation
                           :value  value
                           :dirty? false))
              value)
            value)))

       IEquiv
       (-equiv [this other] (identical? this other))

       IReset
       (-reset!
        [this new-value]
        (let [patch     (core/-make-patch this new-value)
              new-state (core/-state-from-patch this patch)]
          (core/validate-reset (:value state) new-value new-state patch)
          (when patch (set! patches (conj patches patch)))
          (core/-apply-state! this new-state)
          (:value new-state)))

       ISwap
       (-swap! [this f]          (-reset! this (f (:value state))))
       (-swap! [this f a]        (-reset! this (f (:value state) a)))
       (-swap! [this f a b]      (-reset! this (f (:value state) a b)))
       (-swap! [this f a b args] (-reset! this (apply f (:value state) a b args)))

       IWithMeta
       (-with-meta [_ new-meta] (OpsetConvergentRef. actor state patches new-meta validator watches))

       IMeta
       (-meta [_] meta)

       IPrintWithWriter
       (-pr-writer [this writer opts]
                   (-write writer "#object[converge.ref.OpsetConvergentRef ")
                   (pr-writer {:val (-deref this)} writer opts)
                   (-write writer "]"))

       IWatchable
       (-notify-watches
        [this old-value new-value]
        (core/notify-w this watches old-value new-value))
       (-add-watch
        [this k callback]
        (set! watches (assoc watches k callback))
        this)
       (-remove-watch
        [this k]
        (set! watches (dissoc watches k))
        this)

       IHash
       (-hash [this] (goog/getUid this))]))

(defmethod core/make-ref :opset
  [{:keys [initial-value actor meta validator]}]
  (cond
    (map? initial-value)
    (->OpsetConvergentRef actor
                          (->OpsetConvergentState
                           (core/opset core/root-id (ops/make-map))
                           nil
                           nil
                           true)
                          (util/queue)
                          meta
                          validator
                          nil)

    (vector? initial-value)
    (->OpsetConvergentRef actor
                         (->OpsetConvergentState
                          (core/opset core/root-id (ops/make-list))
                          nil
                          nil
                          true)
                         (util/queue)
                         meta
                         validator
                         nil)

    :else
    (throw (ex-info "The initial value of a convergent ref must be either a map or a vector."
                    {:initial-value initial-value}))))

(defmethod core/make-ref-from-ops :opset
  [{:keys [ops actor meta validator]}]
  (->OpsetConvergentRef actor
                       (->OpsetConvergentState ops nil nil true)
                       (util/queue)
                       meta
                       validator
                       nil))

(defmethod core/make-snapshot-ref :opset
  [{:keys [actor meta validator]
    {o :opset
     i* :interpretation}
    :state}]
  (let [id (core/latest-id o)

        i  (or i* (interpret/interpret o))]
    (->OpsetConvergentRef
     actor
     (->OpsetConvergentState (core/opset
                              (core/successor-id id actor)
                              (ops/snapshot id i))
                             i
                             nil
                             true)
     (util/queue)
     meta
     validator
     nil)))
