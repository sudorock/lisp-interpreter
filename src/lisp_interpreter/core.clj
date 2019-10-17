(ns lisp-interpreter.core)

(refer 'clojure.string :only '[split trim])

(defn throw-error [] "Error")

(def functions {"+" +, "-" -, "*" *, "/" /, ">" >, "<" <, ">=" >=})

(def var-ref {"k" 10})

(defn get-atom [el]
      (try
        (Integer/parseInt el)
        (catch Exception e (try
                             (Double/parseDouble el)
                             (catch Exception e (get var-ref el))))))

;(defn parser [s]
;      (if (= (first s) "(")
;        (let [remaining (rest s) start (first (rest s))]
;          (if-let [func (functions start)]
;            (loop [[fst & rst :as all] (rest remaining) args []]
;              (cond
;                (nil? fst) (throw-error)
;                (= fst "(") (let [[res remain] (parser all)] (recur remain (conj args res)))
;                (= fst ")") [(reduce func args) rst]
;                :else (if-let [atm (get-atom fst)] (recur rst (conj args atm)) (throw-error))))
;            (cond
;              (= start "if") (let [[tst rst] (parser (rest remaining)) remain (seq (rest (rest (rest rst))))]
;                               (if tst [(first rst) remain] [(second rst) remain])))))))

(defn resultify [result remaining] (if (empty? remaining) [result nil] [result remaining]))

(defn parser [s]
      (let [start (first s), remaining (rest s), f (functions start), atm (get-atom start)]
        (cond
          (= start ")") (throw-error)
          (= start "(") (let [[func remain] (parser remaining)]
                          (loop [[fst & rst :as all] remain, args []]
                            (cond
                              (= fst ")") (resultify (reduce func args) rst)
                              :else (let [[res left] (parser all)]
                                      (recur left (conj args res))))))
          (some? f) (resultify f remaining)
          (some? atm) (resultify atm remaining))))


(defn str-cleaner [s]
      (filter #(not (= % ""))
              (map trim (split (clojure.string/replace (clojure.string/replace s #"\(" " ( ") #"\)" " ) ") #" "))))

(defn -main [s] (parser (str-cleaner s)))
