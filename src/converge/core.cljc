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
(ns converge.core
  "Datatypes and functions implementing a serializable, Atom-like
  convergent reference type."
  (:require [clojure.data.avl :as avl]
            [converge.util :as util])
  #?(:clj (:import java.util.UUID)))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;;; API

;; TODO trim down to bare semantics, split other ops into different protocols
(defprotocol ConvergentRef
  (-actor [this]
    "Returns the current actor of this convergent ref")
  (-state [this]
    "Returns the current state (an instance of ConvergentState) of this convergent ref")
  (-set-actor! [this actor]
    "Sets this ref's actor to the given value")
  (-log [this]
    "Returns this convergent ref's log")
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
    returns the queue's new value.")
  (-value-from-ops [this ops]
    "Generates a value from the provided ops, not relying on any internal state."))

(defmulti make-ref :backend :default ::default)

(defmethod make-ref ::default
  [options]
  (throw (ex-info "Unknown ConvergentRef backend" options)))

(defmulti make-ref-from-ops :backend :default ::default)

(defmethod make-ref-from-ops ::default
  [options]
  (throw (ex-info "Unknown ConvergentRef backend" options)))

(defmulti make-snapshot-ref :backend :default ::default)

(defmethod make-snapshot-ref ::default
  [options]
  (throw (ex-info "Unknown ConvergentRef backend" options)))

;;;; Implementation

(defn notify-w
  [this watches old-value new-value]
  (doseq [[k w] watches]
    (when w (w k this old-value new-value))))

(defn validate-reset
  [old-value new-value new-state patch]
  (when-not (= new-value (:value new-state))
    (throw (ex-info "Unsupported reference state" {:old-value old-value
                                                   :new-value new-value
                                                   :patch     patch
                                                   :new-state new-state}))))

;;;; Identifiers

(defn ref-id-from-log
  [log]
  (-> log util/first-indexed val :data :id))

(declare id?)

(defrecord Id [^UUID actor ^long counter]
  #?(:clj  Comparable
     :cljs IComparable)
  (#?(:clj  compareTo
      :cljs -compare)
    [_ other]
    (assert (id? other))
    (compare
     [counter actor]
     [(:counter other) (:actor other)])))

(defn make-id
  ([]
   (make-id (util/uuid)))
  ([actor]
   (make-id actor 0))
  ([actor counter]
   (assert (nat-int? counter) "The `counter` of an Id must be an integer")
   (assert (or (nil? actor) (uuid? actor)) "The `actor` of an Id must be a UUID")
   (->Id actor counter)))

(defn id?
  [o]
  (instance? Id o))

(defn successor-id
  ([id]
   (successor-id id (:actor id)))
  ([{:keys [counter]} actor]
   (make-id actor (inc counter))))

(defn latest-id
  [log]
  (some-> log util/last-indexed key))

(defn next-id
  [log actor]
  (if-let [latest (latest-id log)]
    (successor-id latest actor)
    (make-id actor)))

;;;; Operation Log

(defn log
  "An operation log is a sorted map of Id -> Op"
  ([]
   (avl/sorted-map))
  ([& id-ops]
   (apply avl/sorted-map id-ops)))

(defrecord Op [^long action data])

(defn op
  ([action]
   (op action nil))
  ([action data]
   (assert (integer? action) "The `action` of an Op must be an integer")
   (assert (or (nil? data) (map? data)) "The `data` of an Op, if provided, must be a map")
   (->Op action data)))

(def ^:const ROOT -1)

(defn root-op
  [id backend]
  (op ROOT {:id id :backend backend}))

;;;; Patch

(defrecord Patch [source ops])

(defn patch?
  [o]
  (instance? Patch o))

;;;; State

(defrecord ConvergentState [log cache value ^boolean dirty?])
