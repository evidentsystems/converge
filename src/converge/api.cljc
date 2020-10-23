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
(ns converge.api
  "The public API of the Converge library, exposing a convergent
  reference type that acts like a Clojure Atom, with additional
  functions for comparing, merging, and patching these convergent
  refs."
  (:refer-clojure :exclude [ref])
  (:require [converge.opset :as opset]
            [converge.ref :as ref]
            [converge.edn :as edn]
            [converge.util :as util]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

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
  (let [actor* (or actor (util/uuid))

        r
        (cond
          (map? initial-value)
          (ref/->ConvergentRef actor*
                               (ref/->ConvergentState
                                (opset/opset opset/root-id (opset/make-map))
                                nil
                                true)
                               (util/queue)
                               meta
                               validator
                               nil)

          (vector? initial-value)
          (ref/->ConvergentRef actor*
                               (ref/->ConvergentState
                                (opset/opset opset/root-id (opset/make-list))
                                nil
                                true)
                               (util/queue)
                               meta
                               validator
                               nil)

          :else
          (throw (ex-info "The initial value of a convergent ref must be either a map or a vector."
                          {:initial-value initial-value})))]
    @r
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
        actor*         (or actor (util/uuid))
        initial-action (get-in opset* [opset/root-id :action])
        r              (ref/->ConvergentRef actor*
                                            (ref/->ConvergentState opset* nil true)
                                            (util/queue)
                                            meta
                                            validator
                                            nil)]
    @r
    r))

(defn convergent?
  [o]
  (satisfies? ref/IConvergent o))

(defn set-actor!
  [cr actor]
  (ref/-set-actor! cr actor)
  cr)

(defn opset
  [cr]
  (ref/-opset cr))

(defn merge!
  [cr other]
  (let [patch (cond
                (convergent? other)
                (ref/->Patch (opset other))

                (ref/patch? other)
                other

                :else
                (throw (ex-info "Cannot merge! this object into convergent reference"
                                {:ref    cr
                                 :object other})))]
    (ref/-apply-state! cr (ref/-state-from-patch cr patch))
    cr))

(defn squash!
  [cr other]
  (let [spec-opset
        (merge (opset cr)
               (cond
                 (convergent? other)
                 (opset other)

                 (ref/patch? other)
                 (:ops other)

                 :else
                 (throw (ex-info "Cannot merge! this object into convergent reference"
                                 {:ref    cr
                                  :object other}))))]
    (reset! cr (edn/edn spec-opset))
    cr))

(defn peek-patches
  [cr]
  (ref/-peek-patches cr))

(defn pop-patches!
  [cr]
  (let [p (peek-patches cr)]
    (ref/-pop-patches! cr)
    p))

(comment

  (require '[criterium.core :as criterium])

  (def m (ref {}))
  @m

  (def v (ref []))
  @v

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

  (criterium/bench
   (swap! c dissoc :a))
  @c

  )
