(ns clarsec.util)

;;(defn- map-vals [f coll] (into {} (for [[k v] coll] [k (f v)])))

(def max-by (comp first reverse sort-by))
(def min-by (comp first sort-by))

(defmacro def- [symb val] `(def ^:private ~symb ~val))

(defn seq1 [#^clojure.lang.ISeq s]
  (reify clojure.lang.ISeq
    (first [_] (.first s))
    (more [_] (seq1 (.more s)))
    (next [_] (let [sn (.next s)] (and sn (seq1 sn))))
    (seq [_] (let [ss (.seq s)] (and ss (seq1 ss))))
    (count [_] (.count s))
    (cons [_ o] (.cons s o))
    (empty [_] (.empty s))
    (equiv [_ o] (.equiv s o))))
