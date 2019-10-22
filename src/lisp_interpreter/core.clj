(ns lisp-interpreter.core)
(refer 'clojure.string :only '[split trim replace-first join])

(declare parser)

(def variables (atom {}))
(def functions {"+" +, "-" -, "*" *, "/" /, ">" >, "<" <, ">=" >=, "abs" #(max % (- %)), "max" max, "min" min,
                "=" =, "not" not, "sqrt" #(Math/sqrt %)})
(def constants {"true" true, "false" false, "null" nil})

(defn throw-error [] "Error")
(defn str-cleaner [s]
  (filter #(not (= % ""))
          (map trim (split (clojure.string/replace (clojure.string/replace s #"\(" " ( ") #"\)" " ) ") #" "))))

(defn get-atom [el]
  (try (Integer/parseInt el)
       (catch Exception e (try (Double/parseDouble el)
                               (catch Exception e (get @variables el))))))

(defn handle-if [aft-if]
  (let [[tst aft-tst] (parser aft-if), [then aft-then] (parser aft-tst),
        [else aft-else] (parser aft-then), remain (rest aft-else)]
    (if (not (= (first aft-else) ")"))
      (throw-error)
      (if tst [then remain] [else remain]))))

(defn handle-define [aft-def]
  (let [var-name (first aft-def), var-val (parser (rest aft-def)), remain (rest (rest aft-def))]
    (println var-val)
    (if (not (= (first remain) ")"))
      (throw-error)
      (swap! variables assoc var-name (get-atom var-val)))))

(defn handle-quote
  ([aft-quot] (cond
                (= (first aft-quot) "(") (loop [[fst & rst] (rest aft-quot), result "(", cnt 1]
                                           (cond
                                             (zero? cnt) (handle-quote (read-string result) rst)
                                             (= fst "(") (recur rst (str result \space fst \space) (inc cnt))
                                             (= fst ")") (recur rst (str result \space fst \space) (dec cnt))
                                             :else (recur rst (str result \space fst \space) cnt)))
                :else (handle-quote (read-string (first aft-quot)) (rest aft-quot))))
  ([result remain] (if (or (= "(" (first remain)) (nil? remain)) result (throw-error))))

(defn handle-set [aft-set]
  (let [var-name (first aft-set), [var-val remain] (parser (rest aft-set))]
    (if (not (= (first remain) ")"))
      (throw-error)
      (swap! variables assoc var-name var-val))))

(defn handle-lambda [aft-lambda]
  (let [prefix "(quote (fn ", suffix (replace-first (replace-first (join " " aft-lambda) #"\(" "[") #"\)" "]"), exp (str prefix suffix ")")]
    [(eval (eval (read-string exp))) ()]))

(defn special-form [s]
  (condp = (first s)
    "if" (handle-if (rest s))
    "define" (handle-define (rest s))
    "set!" (handle-set (rest s))
    "quote" (handle-quote (rest s))
    "lambda" (handle-lambda (rest s))))

(defn parser [s]
  (let [start (first s), remaining (rest s), f (functions start), atm (get-atom start), const (constants start)]
    (println f atm)
    (cond
      (= start ")") (throw-error)
      (= start "(") (if-let [[func remain] (parser remaining)]
                      (loop [[fst & rst :as all] remain, args []]
                        (cond
                          (nil? fst) (throw-error)
                          (= fst ")") [(apply func args) rst]
                          :else (let [[res remain] (parser all)]
                                  (recur remain (conj args res)))))
                      (special-form remaining))
      (some? f)  [f remaining]
      (some? atm)  [atm remaining]
      (some? const) [const remaining]
      :else nil)))

(defn -main [s] (parser (str-cleaner s)))


;; tokenize the expression on the fly: look for ( and start expression vector, when you meet ) return expression vector
;; evaluate returned expression vector
;; eval: first check for special forms and if not found apply first arg func to the remaining args
