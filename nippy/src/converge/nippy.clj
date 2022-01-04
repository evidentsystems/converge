(ns converge.nippy
  (:require [converge.serialize :as serialize]
            [taoensso.nippy :as nippy])
  (:import [converge.domain ConvergentState]
           [converge.editscript.ref EditscriptConvergentRef]
           [converge.opset.interpret Interpretation]
           [converge.opset.ref OpsetConvergentRef]))

(set! *warn-on-reflection* true)

(nippy/extend-freeze
 ConvergentState :converge/state
 [x out]
 (nippy/freeze-to-out! out (serialize/write-state x)))

(nippy/extend-thaw
 :converge/state
 [in]
 (serialize/read-state (nippy/thaw-from-in! in)))

(nippy/extend-freeze
 Interpretation :opset/interpretation
 [x out]
 (nippy/freeze-to-out! out (serialize/write-interpretation x)))

(nippy/extend-thaw
 :opset/interpretation
 [in]
 (serialize/read-interpretation (nippy/thaw-from-in! in)))

(nippy/extend-freeze
 OpsetConvergentRef :opset/ref
 [x out]
 (nippy/freeze-to-out! out (serialize/write-ref x)))

(nippy/extend-thaw
 :opset/ref
 [in]
 (serialize/read-opset-convergent-ref (nippy/thaw-from-in! in)))

(nippy/extend-freeze
 EditscriptConvergentRef :editscript/ref
 [x out]
 (nippy/freeze-to-out! out (serialize/write-ref x)))

(nippy/extend-thaw
 :editscript/ref
 [in]
 (serialize/read-editscript-convergent-ref (nippy/thaw-from-in! in)))

(comment

  (require '[clojure.java.io :as io]
           '[converge.api :as convergent]
           '[converge.opset.interpret :as interpret])

  (def r (convergent/ref {:foo :bar}))

  (nippy/freeze-to-file "ref.nippy" r)

  (def ref-from-read
    (nippy/thaw-from-file "ref.nippy"))

  (= (convergent/ref-log r)
     (convergent/ref-log ref-from-read))

  (def interpretation
    (interpret/interpret (convergent/ref-log r)))

  (nippy/freeze-to-file "interpretation.nippy" interpretation)

  (def interpretation-from-read
    (nippy/thaw-from-file "interpretation.nippy"))

  (= interpretation interpretation-from-read)

  (def clock (convergent/clock r))

  (nippy/freeze-to-file "clock.nippy" clock)

  (def clock-from-read
    (nippy/thaw-from-file "clock.nippy"))

  (= clock clock-from-read)

  (def r1 (convergent/ref-from-ops (convergent/ref-log r)))
  (swap! r assoc :quux :bar)
  (def patch (convergent/patch-from-clock r (convergent/clock r1)))

  (nippy/freeze-to-file "patch.nippy" patch)

  (def patch-from-read
    (nippy/thaw-from-file "patch.nippy"))

  (= patch patch-from-read)

  (require '[clojure.edn :as edn])

  (def big-tree
    (with-open [pbr (java.io.PushbackReader. (io/reader (io/file "big-tree.edn")))]
      (edn/read pbr)))

  (def big-tree-ref
    (convergent/ref big-tree))

  (nippy/freeze-to-file "big-tree.nippy" big-tree)

  (.length (io/file "big-tree.nippy"))

  (nippy/freeze-to-file "big-tree-ref.nippy" big-tree-ref)

  (.length (io/file "big-tree-ref.nippy"))

  ;;
  )
