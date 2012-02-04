(ns clarsec.helper
  (:use [clarsec core monad thread-expr]))

(defn- m-assoc-1-kv [m-map key m-value]
  (let-bind [m m-map
             value m-value]
            (result (assoc m key value))))

(defn wrap-in-monad [monad-or-val]
  (as-parser monad-or-val))

(defn m-assoc-1-kv-friendly [m-map key m-value]
  (m-assoc-1-kv m-map key (wrap-in-monad m-value)))

(defn m-assoc [m-map key m-value & kvs]
  (-> (wrap-in-monad m-map)
      (for-> [[k v] (concat [[key m-value]] (partition 2 kvs))]
             (m-assoc-1-kv-friendly k v))))

(defn- m-assoc_-sexp
  [m-map key m-value & kvs]
  (let [cmds (for [[k v] (concat [[key m-value]] (partition 2 kvs))]
               (if (= k '_)
                 `(<* ~v)
                 `(m-assoc-1-kv-friendly ~k ~v)))]
    `(-> (wrap-in-monad ~m-map)
         ~@cmds)))

(defmacro m-assoc*
  [& args]
  (apply m-assoc_-sexp {} args))

(defn postfix-generator [many-func]
  (fn postfix-parser [p op]
    (let [flip #(vector %2 %1)]
      (<$> (partial reduce flip) (<$> cons p (many-func op))))))
(defn many-0or1 [p] (<$> #(or % []) (optional p)))
(defn many-1 [p] (<$> #(vector %) p))
(defn many-1+ [p] (<$> #(if (empty? %) nil %) (many p)))
(def postfix-0+ (postfix-generator many))
(def postfix-0or1 (postfix-generator many-0or1))
(def postfix-1+ (postfix-generator many-1+))

(defn infix-gen [p]
  (letfn [(reduce3
           [afn [e1 e2 e3 & rest :as coll]]
           (if (< (count coll) 2)
             coll
             (recur afn (cons (afn e1 e2 e3) rest))))

          (unbox-singletons
           [coll]
           (if (>= 1 (count coll)) (first coll) coll))]
    
    (<$> #(->> % (reduce3 (fn [a op b] [op a b])) (unbox-singletons))
         p)))

(defn infix-0+ [p op]
  (letfn [(manycat [p] (<$> #(apply concat %) (many p)))]
    (infix-gen (<$> cons p (manycat [op p])))))

(defn infix-0or1 [lhs op rhs] (infix-gen (<$> cons lhs (optional [op rhs]))))
(defn infix-1 [lhs op rhs] (infix-gen [lhs op rhs]))


;; Docs: http://legacy.cs.uu.nl/daan/download/parsec/parsec.html#chainl1
(defn prefix-0+ [p op]  
  (<|> [op (lazy (prefix-0+ p op))]
       p))

(defn can-parse? [p]
  (option false (<$> (fn [x] true) p)))

(defn invalid-parse [p msg]
  (<$> (fn [x] (throw (new Exception msg))) p))

(defmacro declare-parsers [& symbols]
  `(do (declare ~@symbols)
       ~@(for [s symbols]
           `(def ~s (lazy ~s)))))
