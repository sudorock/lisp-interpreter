(ns lisp-interpreter.core
  (:require [clojure.string :refer [split trim replace-first join escape]]))
(declare evaluate)

(defn throw-error [] (throw (Exception. "Parse Error")))

(def envs (atom {:g {'+               #(apply + %)
                     '-               #(apply - %)
                     '*               #(apply * %)
                     '/               #(apply / %)
                     '>               #(apply > %)
                     '<               #(apply < %)
                     '>=              #(apply >= %)
                     '<=              #(apply <= %)
                     '=               #(apply = %)
                     'not             #(apply not %)
                     'max             #(apply max %)
                     'min             #(apply min %)
                     'sqrt            (fn [a] (apply #(Math/sqrt %) a))
                     'expt            (fn [a] (apply #(Math/pow %1 %2) a))
                     'round           (fn [a] (apply #(Math/round %) a))
                     'abs             (fn [a] (apply #(max % (- %)) a))
                     'number?         #(apply number? %)
                     'procedure?      #(apply fn? %)
                     'symbol?         #(apply string? %)
                     'equal?          #(apply = %)
                     'list            identity
                     'car             #(apply first %)
                     'cdr             #(apply next %)
                     'cons            (fn [a] (apply #(cons %1 %2) a))
                     'apply           #(apply %1 %&)
                     'map             (fn [[f & bdy]] (map f (map vector (first bdy))))
                     'pi              3.141592653589793
                     (symbol "true")  true
                     (symbol "false") false}}))
(def parent (atom {}))

(def macros (atom {}))

(defn env-find [sym env] (when-let [cur (@envs env)] (if-let [vl (cur sym)] vl (env-find sym (@parent env)))))

(defn atomize [el]
  (try (Integer/parseInt el) (catch Exception e (try (Double/parseDouble el) (catch Exception e (symbol el))))))

(defn evaluate [exp env]
  (cond
    ((some-fn number? boolean? fn?) exp) exp
    (symbol? exp) (env-find exp env)
    (some #(= (get exp 0) %) ['set! 'define]) (do (swap! envs assoc-in [env (exp 1)] (evaluate (exp 2) env)))
    (= (get exp 0) 'if) (if (evaluate (exp 1) env) (evaluate (exp 2) env) (evaluate (exp 3) env))
    (= (get exp 0) 'quote) (exp 1)
    (= (get exp 0) 'begin) (last (map #(evaluate % env) (subvec exp 1)))
    (= (get exp 0) 'lambda) (fn [args] (let [cur-env (keyword (gensym))]
                                         (swap! parent assoc cur-env env)
                                         (swap! envs assoc cur-env (zipmap (exp 1) args))
                                         (evaluate (exp 2) cur-env)))
    (= (get exp 0) 'defn-macro) (swap! macros assoc (exp 1) {"args" (exp 2) "body" (exp 3)})
    (fn? (env-find (get exp 0) env)) ((env-find (get exp 0) env) (map #(evaluate % env) (subvec exp 1)))
    (fn? (get exp 0)) ((get exp 0) (map #(evaluate % env) (subvec exp 1)))
    :else (if (coll? exp) exp (throw-error))))

(defn parse [s eval?]
  (cond
    (re-find #"^\)" s) (throw-error)
    (and eval? (re-find #"^\(\s*(?:lambda|quote|defn-macro)\s+" s)) (let [[res rmn] (parse s false)] [(evaluate res :g) (trim rmn)])
    (re-find #"^\(" s) (loop [rst (trim (replace-first s #"^\(" "")), exp []]
                         (cond
                           (empty? rst) (throw-error)
                           (re-find #"^\)" rst) [(if eval? (evaluate exp :g) exp) (trim (replace-first rst #"^\)" ""))]
                           :else (when-let [[res rmn] (parse rst eval?)] (recur (trim rmn) (conj exp res)))))
    :else (if-let [res (re-find #"^\S*[^\(\)\s]+" s)] [(atomize (trim res)) (trim (subs s (count res)))] s)))

;; Interpreter and REPL
(defn interpret [s] (parse (trim s) true))
(defn -main []
  (do (printf "\033[0;1mcljisp ~ \u03BB  \033[34;1m") (flush)
      (let [[res rmn] (interpret (read-line))]
        (if (empty? rmn) (do (printf "%s\n" (pr-str res)) (-main)) (throw-error)))))

;; issues true not parsing properly in if