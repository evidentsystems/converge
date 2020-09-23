(ns converge.edn
  "API for interpreting an OpSet as EDN data."
  (:require [clojure.walk :as walk]
            [converge.opset :as opset]
            [converge.interpret :as interpret]))

(set! *warn-on-reflection* true)

(defprotocol ReplaceIdValues
  (replace-id-values [this index]))

(extend-protocol ReplaceIdValues
  #?(:cljs default
     :clj  java.lang.Object)
  (replace-id-values [this _index]
    this)

  nil
  (replace-id-values [this _index]
    this)

  #?(:cljs MapEntry
     :clj  java.util.Map$Entry)
  (replace-id-values [[k v :as entry] _index]
    (if (opset/id? k)
      [(vary-meta k assoc :key? true) v]
      entry))

  converge.opset.Id
  (replace-id-values [this index]
    (if (:key? (meta this))
      (vary-meta this dissoc :key?)
      (vary-meta (get index this)
                 assoc :converge/id this))))

(defn- build-list
  [head-id list-map list-links]
  (loop [prev head-id
         list ^{:converge/insertions []} []]
    (let [next      (get list-links prev)
          element   (get list-map next)
          next-list (if element
                      (-> list
                          (conj element)
                          (vary-meta update :converge/insertions conj next))
                      list)]
      (if (= next interpret/list-end-sigil)
        next-list
        (recur next next-list)))))

(defn object-index
  [opset {:keys [elements list-links]}]
  (let [index (reduce-kv (fn [agg _id {:keys [object key value]}]
                           (let [v (get-in opset [value :data :value]
                                           value)]
                             (cond-> agg
                               ;; ensure empty collections are included in object-index
                               (and (opset/id? v)
                                    (some-> opset (get v) :action #{:make-list :make-map})
                                    (empty? (get agg v)))
                               (assoc v {})

                               true
                               (assoc-in [object key] v))))
                         {}
                         elements)]
    (reduce-kv (fn [agg id object]
                 (let [op (get opset id)]
                   (if (= (:action op) :make-list)
                     (assoc agg id (build-list id object list-links))
                     agg)))
               index
               index)))

(defn edn
  [opset]
  (cond
    (= (count opset) 0)
    nil

    (= (count opset) 1)
    (vary-meta
     (case (some-> opset first val :action)
       :make-map  {}
       :make-list []
       (throw (ex-info "Invalid OpSet" {:opset opset})))
     assoc :converge/id opset/root-id)

    :else
    (let [interpretation (interpret/interpret opset)
          index          (object-index opset interpretation)]
      (some-> (walk/prewalk #(replace-id-values % index)
                            index)
              (get opset/root-id (case (some-> opset first val :action)
                                   :make-map  {}
                                   :make-list []
                                   (throw (ex-info "Invalid OpSet" {:opset opset}))))
              (vary-meta assoc :converge/id opset/root-id)))))

(comment

  (do

    (def id1 opset/root-id)
    (def id2 (opset/make-id))
    (def id3 (opset/successor-id id2))
    (def id4 (opset/successor-id id3))
    (def id5 (opset/successor-id id4))
    (def id6 (opset/successor-id id5))
    (def id7 (opset/successor-id id6))
    (def id8 (opset/successor-id id7))
    (def id9 (opset/successor-id id8))
    (def id10 (opset/successor-id id9))
    (def id11 (opset/successor-id id10))
    (def id12 (opset/successor-id id11))
    (def id13 (opset/successor-id id12))
    (def id14 (opset/successor-id id13))
    (def id15 (opset/successor-id id14))

    )

  (def o (atom (opset/opset id1 (opset/make-map))))

  (do

    (swap! o assoc id2  (opset/make-value :bar))
    (swap! o assoc id3  (opset/assign id1 :foo id2))
    (swap! o assoc id4  (opset/make-value :quux))
    (swap! o assoc id5  (opset/assign id1 :baz id4))
    (swap! o assoc id6  (opset/remove id1 :foo))
    (swap! o assoc id7  (opset/make-list))
    (swap! o assoc id8  (opset/assign id1 :a-list id7))
    (swap! o assoc id9  (opset/insert id7))
    (swap! o assoc id10 (opset/make-value :item1))
    (swap! o assoc id11 (opset/assign id7 id9 id10))
    (swap! o assoc id12 (opset/insert id7))
    (swap! o assoc id13 (opset/make-value :item2))
    (swap! o assoc id14 (opset/assign id7 id12 id13))
    (swap! o assoc id15 (opset/remove id7 id9))

    )

  ;; => {:baz :quux :a-list [:item2]}

  (binding [*print-meta* true]
    (prn (edn @o)))

  (meta (edn @o))

  (def i (interpret/interpret @o))

  )
