(ns lisp-interpreter.core)
(refer 'clojure.string :only '[split trim replace-first join escape])
(declare parse evaluate)
(defn throw-error [] (throw (Exception. "Parse Error")))
(def const {"+" #(apply + %), "-" #(apply - %), "*" #(apply * %), "/" /, ">" #(apply > %), "<" <, ">=" >=, "abs" #(max % (- %)), "max" max, "min" min,
            "=" =, "not" not, "sqrt" #(Math/sqrt %), "true" true, "false" false})

(def env (atom {}))

(defn atomize [el]
  (try (Integer/parseInt el)
       (catch Exception e
         (try (Double/parseDouble el)
              (catch Exception e (if-let [cnst (get const el)] cnst el))))))

(defn handle-lambda [form]
  (fn [args]
    (let [zip (zipmap (form 1) args)]
      (println args zip)
      (when (some? args) (evaluate (mapv #(if (zip %) (zip %) %) (form 2)))))))

(defn evaluate [form]
  (cond
    (or (boolean? form) (number? form)) form
    (string? form) (@env form)
    (= (form 0) "define") (swap! env assoc (form 1) (evaluate (form 2)))
    (= (form 0) "if") (if (evaluate (form 1)) (evaluate (form 2)) (evaluate (form 3)))
    (= (form 0) "lambda") (println "hello")
    (fn? (@env (form 0))) ((@env (form 0)) (map evaluate (subvec form 1)))
    :else ((form 0) (map evaluate (subvec form 1)))))

;(defn parse-lambda [s exp]
;  (cond
;    (re-find #"^\)" s) (throw-error)
;    (re-find #"^\(" s) (loop [rst (trim (replace-first s #"^\(" "")), exp []]
;                         (cond
;                           (empty? rst) (throw-error)
;                           (re-find #"^\)" rst) [exp (trim (replace-first rst #"^\)" ""))]
;                           :else (when-let [[res rmn] (parse-lambda rst exp)] (recur (trim rmn) (conj exp res)))))
;    :else (if-let [res (re-find #"^\S*[^\(\)\s]+" s)] [(atomize (trim res)) (trim (subs s (count res)))] s)))

(defn parse [s]
  (cond
    (re-find #"^\)" s) (throw-error)
    (re-find #"^\(" s) (loop [rst (trim (replace-first s #"^\(" "")), exp []]
                         (cond
                           (empty? rst) (throw-error)
                           ;(re-find #"^lambda\s+" rst) (parse-lambda rst 1)
                           (re-find #"^\)" rst) [(evaluate exp) (trim (replace-first rst #"^\)" ""))]
                           :else (when-let [[res rmn] (parse rst)] (recur (trim rmn) (conj exp res)))))
    :else (if-let [res (re-find #"^\S*[^\(\)\s]+" s)] [(atomize (trim res)) (trim (subs s (count res)))] s)))

(defn parse-only [s]
  (cond
    (re-find #"^\)" s) (throw-error)
    (re-find #"^\(" s) (loop [rst (trim (replace-first s #"^\(" "")), exp []]
                         (cond
                           (empty? rst) (throw-error)
                           (re-find #"^\)" rst) [exp (trim (replace-first rst #"^\)" ""))]
                           :else (when-let [[res rmn] (parse-only rst)] (recur (trim rmn) (conj exp res)))))
    :else (if-let [res (re-find #"^\S*[^\(\)\s]+" s)] [(atomize (trim res)) (trim (subs s (count res)))] s)))


(defn -main [s] (parse (trim s)))

















;
;(def functions {"+" +, "-" -, "*" *, "/" /, ">" >, "<" <, ">=" >=, "abs" #(max % (- %)), "max" max, "min" min,
;                "=" =, "not" not, "sqrt" #(Math/sqrt %)})
;
;
;
;
;
;
;
;(declare parser)
;(def variables (atom {}))
;(def constants {"true" true, "false" false, "null" nil})
;
;(defn throw-error [] "Error")
;(defn str-cleaner [s]
;  (filter #(not (= % ""))
;          (map trim (split (clojure.string/replace (clojure.string/replace s #"\(" " ( ") #"\)" " ) ") #" "))))
;
;(defn get-atom [el]
;  (try (Integer/parseInt el)
;       (catch Exception e (try (Double/parseDouble el)
;                               (catch Exception e (get @variables el))))))
;
;(defn handle-if [aft-if]
;  (let [[tst aft-tst] (parser aft-if), [then aft-then] (parser aft-tst),
;        [else aft-else] (parser aft-then), remain (rest aft-else)]
;    (if (not (= (first aft-else) ")"))
;      (throw-error)
;      (if tst [then remain] [else remain]))))
;
;(defn handle-define [aft-def]
;  (let [var-name (first aft-def), var-val (parser (rest aft-def)), remain (rest (rest aft-def))]
;    (println var-val)
;    (if (not (= (first remain) ")"))
;      (throw-error)
;      (swap! variables assoc var-name (get-atom var-val)))))
;
;(defn handle-quote
;  ([aft-quot] (cond
;                (= (first aft-quot) "(") (loop [[fst & rst] (rest aft-quot), result "(", cnt 1]
;                                           (cond
;                                             (zero? cnt) (handle-quote (read-string result) rst)
;                                             (= fst "(") (recur rst (str result \space fst \space) (inc cnt))
;                                             (= fst ")") (recur rst (str result \space fst \space) (dec cnt))
;                                             :else (recur rst (str result \space fst \space) cnt)))
;                :else (handle-quote (read-string (first aft-quot)) (rest aft-quot))))
;  ([result remain] (if (or (= "(" (first remain)) (nil? remain)) result (throw-error))))
;
;(defn handle-set [aft-set]
;  (let [var-name (first aft-set), [var-val remain] (parser (rest aft-set))]
;    (if (not (= (first remain) ")"))
;      (throw-error)
;      (swap! variables assoc var-name var-val))))
;
;(defn handle-lambda [aft-lambda]
;  (let [prefix "(quote (fn ", suffix (replace-first (replace-first (join " " aft-lambda) #"\(" "[") #"\)" "]"), exp (str prefix suffix ")")]
;    [(eval (eval (read-string exp))) ()]))
;
;(defn special-form [s]
;  (condp = (first s)
;    "if" (handle-if (rest s))
;    "define" (handle-define (rest s))
;    "set!" (handle-set (rest s))
;    "quote" (handle-quote (rest s))
;    "lambda" (handle-lambda (rest s))))
;
;(defn parser [s]
;  (let [start (first s), remaining (rest s), f (functions start), atm (get-atom start), const (constants start)]
;    (println f atm)
;    (cond
;      (= start ")") (throw-error)
;      (= start "(") (if-let [[func remain] (parser remaining)]
;                      (loop [[fst & rst :as all] remain, args []]
;                        (cond
;                          (nil? fst) (throw-error)
;                          (= fst ")") [(apply func args) rst]
;                          :else (let [[res remain] (parser all)]
;                                  (recur remain (conj args res)))))
;                      (special-form remaining))
;      (some? f)  [f remaining]
;      (some? atm)  [atm remaining]
;      (some? const) [const remaining]
;      :else nil)))
;
;(defn -main [s] (parser (str-cleaner s)))


;; tokenize the expression on the fly: look for ( and start expression vector, when you meet ) return expression vector
;; evaluate returned expression vector
;; eval: first check for special forms and if not found apply first arg func to the remaining args


;(re-find #"\S+" s)

;(?:(?:^\s*[\(\)])|(?:[[^\(\)]\S+]))

;(?:^\s*[\(\)]|[[^\(\)]\S]+)