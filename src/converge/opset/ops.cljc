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

(def ^:const MAKE_MAP 0)
(def ^:const MAKE_VECTOR 1)
(def ^:const MAKE_SET 2)
(def ^:const MAKE_LIST 3)
(def ^:const MAKE_VALUE 4)
(def ^:const INSERT 5)
(def ^:const ASSIGN 6)
(def ^:const REMOVE 7)
(def ^:const SNAPSHOT 8)

(defn make-map
  []
  (core/op MAKE_MAP))

(defn make-vector
  []
  (core/op MAKE_VECTOR))

(defn make-set
  []
  (core/op MAKE_SET))

(defn make-list
  []
  (core/op MAKE_LIST))

(defn make-value
  [value]
  (core/op MAKE_VALUE {:value value}))

(defn insert
  [after]
  (assert (core/id? after) "`after` must be an Id")
  (core/op INSERT {:after after}))

(defn assign
  [entity attribute value]
  (assert (core/id? entity) "`entity` must be an Id")
  (core/op ASSIGN {:entity entity :attribute attribute :value value}))

(defn remove
  [entity attribute]
  (assert (core/id? entity) "`entity` must be an Id")
  (core/op REMOVE {:entity entity :attribute attribute}))

(defn snapshot
  [log-hash interpretation]
  (core/op SNAPSHOT {:log-hash log-hash :interpretation interpretation}))
