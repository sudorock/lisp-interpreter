(ns lisp-interpreter.core)

(refer 'clojure.string :only '[split trim])

(declare parser)

(defn throw-error [] "Error")

(def functions {"+" +, "-" -, "*" *, "/" /, ">" >, "<" <, ">=" >=})

(def var-ref {"k" 10})

(defn get-atom [el]
      (try
        (Integer/parseInt el)
        (catch Exception e (try
                             (Double/parseDouble el)
                             (catch Exception e (get var-ref el))))))

(defn resultify [result remaining] (if (empty? remaining) [result nil] [result remaining]))

(defn get-if-result [aft-if]
      (let [[tst aft-tst] (parser aft-if), [then aft-then] (parser aft-tst),
            [else aft-else] (parser aft-then), remain (rest aft-else)]
        (if (not (= (first aft-else) ")"))
          (throw-error) (if tst (resultify then remain) (resultify else remain)))))

(defn parser [s]
      (let [start (first s), remaining (rest s), f (functions start), atm (get-atom start)]
        (cond
          (= start ")") (throw-error)
          (= start "(") (if-let [[func remain] (parser remaining)]
                          (loop [[fst & rst :as all] remain, args []]
                            (cond
                              (nil? fst) (throw-error)
                              (= fst ")") (resultify (reduce func args) rst)
                              :else (let [[res remain] (parser all)]
                                      (recur remain (conj args res)))))
                          (cond
                            (= (first remaining) "if") (get-if-result (rest remaining))))
          (some? f) (resultify f remaining)
          (some? atm) (resultify atm remaining)
          :else nil)))


(defn str-cleaner [s]
      (filter #(not (= % ""))
              (map trim (split (clojure.string/replace (clojure.string/replace s #"\(" " ( ") #"\)" " ) ") #" "))))

(defn -main [s] (parser (str-cleaner s)))
