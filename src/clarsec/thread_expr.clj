;; OPEN SOURCE CODE HERE:
;; https://github.com/pallet/thread-expr/blob/develop/src/pallet/thread_expr.clj

(ns clarsec.thread-expr)

(defn- for-
  [threader arg seq-exprs body]
  `(reduce #(%2 %1)
             ~arg
             (for ~seq-exprs
               (fn [arg#] (~threader arg# ~@body)))))

(defmacro for->
  "Apply a thread expression to a sequence.
   eg.
      (-> 1
        (for-> [x [1 2 3]]
          (+ x)))
   => 7"
  [arg seq-exprs & body]
  (for- 'clojure.core/-> arg seq-exprs body))

