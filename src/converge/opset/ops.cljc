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
(ns converge.opset.ops
  (:refer-clojure :exclude [remove])
  (:require [converge.core :as core]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;;; Operations

(def ^:const MAKE_MAP 0)
(def ^:const MAKE_VECTOR 1)
(def ^:const MAKE_SET 2)
(def ^:const MAKE_LIST 3)
(def ^:const MAKE_VALUE 4)
(def ^:const INSERT 5)
(def ^:const ASSIGN 6)
(def ^:const REMOVE 7)
(def ^:const SNAPSHOT 8)

(defrecord Op [^long action data])

(defn op
  ([action]
   (op action nil))
  ([action data]
   (assert (integer? action) "The `action` of an Op must be an integer")
   (assert (or (nil? data) (map? data)) "The `data` of an Op, if provided, must be a map")
   (->Op action data)))

(defn make-map
  []
  (op MAKE_MAP))

(defn make-vector
  []
  (op MAKE_VECTOR))

(defn make-set
  []
  (op MAKE_SET))

(defn make-list
  []
  (op MAKE_LIST))

(defn make-value
  [value]
  (op MAKE_VALUE {:value value}))

(defn insert
  [after]
  (assert (core/id? after) "`after` must be an Id")
  (op INSERT {:after after}))

(defn assign
  [entity attribute value]
  (assert (core/id? entity) "`entity` must be an Id")
  (op ASSIGN {:entity entity :attribute attribute :value value}))

(defn remove
  [entity attribute]
  (assert (core/id? entity) "`entity` must be an Id")
  (op REMOVE {:entity entity :attribute attribute}))

(defn snapshot
  [as-of interpretation]
  (assert (core/id? as-of) "`as-of` must be an Id")
  (op SNAPSHOT {:as-of as-of :interpretation interpretation}))
