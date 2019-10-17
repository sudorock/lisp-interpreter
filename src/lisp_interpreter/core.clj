(ns lisp-interpreter.core)

(refer 'clojure.string :only '[split trim])

(declare parser)

(def variables (atom {}))
(def functions {"+" +, "-" -, "*" *, "/" /, ">" >, "<" <, ">=" >=, "abs" #(max % (- %))})

(defn throw-error [] "Error")
(defn resultify [result remaining] (if (empty? remaining) [result nil] [result remaining]))

(defn get-atom [el]
      (try
        (Integer/parseInt el)
        (catch Exception e (try
                             (Double/parseDouble el)
                             (catch Exception e (get @variables el))))))

(defn handle-if [aft-if]
      (let [[tst aft-tst] (parser aft-if), [then aft-then] (parser aft-tst),
            [else aft-else] (parser aft-then), remain (rest aft-else)]
        (if (not (= (first aft-else) ")"))
          (throw-error)
          (if tst (resultify then remain) (resultify else remain)))))

(defn handle-define [aft-def]
      (let [var-name (first aft-def) var-val (second aft-def) remain (rest (rest aft-def))]
        (if (not (= (first remain) ")"))
          (throw-error)
          (swap! variables assoc var-name (get-atom var-val)))))

(defn special-form [s]
      (condp = (first s)
        "if" (handle-if (rest s))
        "define" (handle-define (rest s))))

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
                          (special-form remaining))
          (some? f) (resultify f remaining)
          (some? atm) (resultify atm remaining)
          :else nil)))


(defn str-cleaner [s]
      (filter #(not (= % ""))
              (map trim (split (clojure.string/replace (clojure.string/replace s #"\(" " ( ") #"\)" " ) ") #" "))))

(defn -main [s] (parser (str-cleaner s)))