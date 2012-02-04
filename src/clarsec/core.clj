(ns clarsec.core
  (:use [clarsec monad util  error-report])
  (:require [clarsec [error-report :as error-report]]))

(defn consumed? [{:keys [type]}] (= type :consumed))
(defn failed? [{:keys [type]}] (= type :failed))
(defn failed [] {:type :failed})
(defn consumed [value rest] {:type :consumed :value value :rest rest})

(defn make-parser
  ([parser-fn] (make-monad `Parser parser-fn))
  ([name parser-fn]
     (assoc (make-monad `Parser parser-fn)
       :name name)))

(defn match-fn->parser [input->value value->count]
  (make-parser
   (fn [input]
     (if-let [val (input->value input)]
       (consumed val (subs input (value->count val)))
       (failed)))))

(defn result [v]
  (return `Parser v))
(defn <$> [f & parsers]
  (bind (m-sequence parsers)
        (fn [v]
          (make-parser
           (fn [strn]               
             (if-let [v$ (apply f v)]
               (consumed v$ strn)
               (failed)))))))
(def m-apply <$>)
(defn m-reduce [f & rest] (apply reduce #(<$> f %1 %2) rest))
(defn- m-vector [& parsers]
  (m-reduce conj (result []) parsers))

(extend-type String
  IParser (get-monad [^String this]
                     (match-fn->parser (fn [^String i] (if (.startsWith i this) this))
                                       count)))

(extend-type java.util.regex.Pattern
  IParser (get-monad [this]                     
                     (match-fn->parser #(re-find (re-pattern (str "^(?:" this ")")) %)
                                       (let [root-match-group #(if (vector? %) (first %) %)]
                                         (comp count root-match-group)))))

(extend-type clojure.lang.PersistentVector
  IParser (get-monad [this] (apply m-vector this)))

(extend-type clojure.lang.Keyword
  IParser (get-monad [this] (result this)))
(extend-type clojure.lang.Symbol
  IParser (get-monad [this] (result this)))
(extend-type clojure.lang.PersistentArrayMap
  IParser (get-monad [this] (result this)))

(defn as-parser [supported-parser-type]
  (get-monad supported-parser-type))

(defn run-parser-fn [m input]
  (let [result ((monad (as-parser m)) input)]
    (when (consumed? result)
      (error-report/update! result))
    result))

(defn parse$ [parser input]  
  (error-report/with-new-report
   input
   (run-parser-fn parser input)))

(defmethod return `Parser [t x]
  (make-parser (fn p-return [strn] (consumed x strn))))

(defmethod bind `Parser [m v->m]
  (make-parser
   (fn monad-parser-fn [strn]
     (let [pfn-result (run-parser-fn m strn)]       
       (if (consumed? pfn-result)         
         (run-parser-fn (v->m (:value pfn-result)) (:rest pfn-result))
         pfn-result)))))

(defn force-during-parse [d]
  (make-parser (fn [strn] ((monad (force d)) strn))))
(defmacro lazy [& body] `(force-during-parse (delay ~@body)))
(def fail (make-parser (fn p-fail [strn] (failed))))


(defn <|> [& parsers]
  (if-not (seq parsers)
    fail
    (make-parser
     (fn either [strn]
       (let [p-results (->> parsers (seq1) (map #(run-parser-fn % strn)))]
         (or (first (drop-while failed? p-results))
             (failed)))))))




;; ---------------------------------------------------------------------------
;; The rest of this file is open source:
;; ---------------------------------------------------------------------------

(defn optional [p]
  (<|> p (result nil)))

(defn option [default p]
  (<|> p (result default)))

(defn many [p]
  (make-parser   
   (partial (fn [ret strn]
              (let [pfn-result (run-parser-fn p strn)]          
                (if (and (consumed? pfn-result) (pos? (count (:rest pfn-result))))
                  (recur (conj ret (:value pfn-result)) (:rest pfn-result))
                  (consumed (into '() ret) strn)))) [])))

(defn many1 [parser]
  (let-bind [a parser
             as (lazy (many parser))]
            (result (concat [a] as))))



(defn end-by-m [f p sep]
  (f (let-bind [r p
                _ sep]
       (result r))))

(defn end-by [p sep] (end-by-m many p sep))

(defn end-by-1 [p sep] (end-by-m many1 p sep))

(defn sep-by-1 [p sep]
  (let-bind [x p
             xs (many (>> sep p))]
            (result (cons x xs))))

(defn sep-by [p sep]
  (<|> (sep-by-1 p sep) (result ())))

(defn between
  ([open close] (between open close (result nil)))
  ([open close p]
      (let-bind [_ open
                 x p
                 _ close]
                (result x))))



;; Deprecated
(defn string [strn] (as-parser strn))

(defn- no-remaining [p]
  (<* p (make-parser
         (fn [input] (if (empty? input)
                      (result nil)
                      (fail))))))

(defn chars-as-str [ch] (<$> #(apply str %) (many ch)))

(defn- lexeme [p] (*> #"[\s\n\r]+" p))
(defn- symb [name] (lexeme (string name)))
