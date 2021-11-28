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
(ns converge.editscript.ref
  (:require [clojure.data.avl :as avl]
            [editscript.core :as e]
            [converge.core :as core]
            [converge.util :as util]
            [converge.editscript.ops :as ops])
  #?(:clj (:import [clojure.lang IAtom IReference IRef])))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(defn apply-patch
  [value {:keys [action data]}]
  (condp = action
    ops/EDIT
    (e/patch value (e/edits->script (:edits data)))

    ops/SNAPSHOT
    (:value data)

    :else
    value))

(defn value-from-opset
  [opset]
  (reduce apply-patch nil (vals opset)))

(deftype EditscriptConvergentRef #?(:clj  [^:volatile-mutable actor
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
    (when (ifn? validator)
      (assert (validator new-value) "Validator rejected reference state"))
    (let [edits (e/get-edits (e/diff (:value state) new-value {:str-diff? true}))]
      (when (pos? (count edits))
        (core/->Patch (avl/sorted-map
                       (core/next-id (:opset state) actor)
                       (ops/edit edits))))))
  (-state-from-patch [_ patch]
    (if (core/patch? patch)
      (let [{:keys [ops]}
            patch

            new-opset
            (into (:opset state) ops)]
        (core/->ConvergentState new-opset
                                nil
                                (if (:dirty? state)
                                  (value-from-opset new-opset)
                                  (reduce apply-patch (:value state) (vals ops)))
                                false))
      state))
  (-peek-patches [_] (peek patches))
  (-pop-patches! [_] (set! patches (pop patches)))
  (-value-from-ops [_ ops]
    (value-from-opset ops))

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
       ;; TODO: refactor this out, and only do these impls in one place
       (deref
        [_]
        (let [{:keys [opset value dirty?] :as s}
              state]
          (if dirty?
            (let [new-value (value-from-opset opset)]
              (set! state
                    (assoc s
                           :value  new-value
                           :dirty? false))
              new-value)
            value)))
       (setValidator
        [_ f]
        (when (ifn? f)
          (assert (f (:value state)) "Validator rejected reference state")
          (set! validator f)))
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
       ;; TODO: refactor this out, and only do these impls in one place
       (-deref
        [_]
        (let [{:keys [opset value dirty?] :as s}
              state]
          (if dirty?
            (let [new-value (value-from-opset opset)]
              (set! state
                    (assoc s
                           :value  new-value
                           :dirty? false))
              new-value)
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

(defmethod core/make-ref :editscript
  [{:keys [initial-value actor meta validator]}]
  (->EditscriptConvergentRef actor
                             (core/->ConvergentState
                              (core/opset core/root-id (ops/snapshot core/root-id initial-value))
                              nil
                              nil
                              true)
                             (util/queue)
                             meta
                             validator
                             nil))

(defmethod core/make-ref-from-ops :editscript
  [{:keys [ops actor meta validator]}]
  (->EditscriptConvergentRef actor
                             (core/->ConvergentState ops nil nil true)
                             (util/queue)
                             meta
                             validator
                             nil))

(defmethod core/make-snapshot-ref :editscript
  [{:keys [actor meta validator]
    {o :opset
     v :value}
    :state}]
  (let [id (core/latest-id o)]
    (->EditscriptConvergentRef
     actor
     (core/->ConvergentState (core/opset
                              (core/successor-id id actor)
                              (ops/snapshot id v))
                             nil
                             nil
                             true)
     (util/queue)
     meta
     validator
     nil)))
