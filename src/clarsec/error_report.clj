(ns clarsec.error-report)

(defn- error-line-number [initial-input shortest-rest-len]
  (let [index-of-err (- (count initial-input) shortest-rest-len)]        
    (->> (subs initial-input 0 index-of-err)
         (filter #{\newline})
         (count)
         (inc))))

(declare ^:dynamic *shortest-rest-len*
         ^:dynamic *initial-input*)

(defn generate []
  {:line-number (error-line-number *initial-input* @*shortest-rest-len*)})

(defmacro with-new-report [initial-input & body]
  `(binding [*shortest-rest-len* (atom (count ~initial-input))
             *initial-input* ~initial-input]     
     (let [result# (try ~@body
                        (catch Exception e#
                          (merge {:type :failed :exception e#})))]
       (if (= :failed (:type result#))
         (merge result# (generate))
         result#))))

(defn update! [parser-result]
  (when (bound? #'*shortest-rest-len*)
    (swap! *shortest-rest-len* min (count (:rest parser-result)))))