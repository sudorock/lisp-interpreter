(ns lisp-interpreter.core
  (:require [clojure.string :refer [split trim replace-first join escape]]))
(declare evaluate)
(defn throw-error [] (throw (Exception. "Parse Error")))
(defn uuid [] (str (java.util.UUID/randomUUID)))

(def envs (atom {:global {}}))
(def parent (atom {}))
(def const {"+"       #(apply + %), "-" #(apply - %), "*" #(apply * %), "/" #(apply / %), ">" #(apply > %), "<" #(apply < %),
            ">="      #(apply >= %), "<=" #(apply <= %), "=" #(apply = %), "not" #(apply not %), "max" #(apply max %),
            "min"     #(apply min %), "sqrt" (fn [a] (apply #(Math/sqrt %) a)), "expt" (fn [a] (apply #(Math/pow %1 %2) a)),
            "round"   (fn [a] (apply #(Math/round %) a)), "abs" (fn [a] (apply #(max % (- %)) a)),
            "number?" #(apply number? %), "procedure?" #(apply fn? %), "symbol?" #(apply string? %),
            "car"     (fn [a] (apply #(first %) a)), "cdr" (fn [a] (apply #(rest %) a)),
            "true"    true, "false" false,
            "pi"      3.141592653589793})

(defn env-find [sym env] (when-let [cur (@envs env)] (if-let [vl (cur sym)] vl (env-find sym (@parent env)))))

(defn atomize [el]
  (try (Integer/parseInt el)
       (catch Exception e (try (Double/parseDouble el)
                               (catch Exception e (if-let [cnst (get const el)] cnst el))))))

(defn handle-lambda [exp env]
  (fn [args]
    (let [cur-env (uuid)]
      (swap! parent assoc cur-env env)
      (swap! envs assoc cur-env (zipmap (exp 1) args))
      (evaluate (exp 2) cur-env))))

(defn evaluate [exp env]
  (cond
    ((some-fn number? boolean? fn?) exp) exp
    (string? exp) (env-find exp env)
    (or (= (exp 0) "define") (= (exp 0) "set!")) (swap! envs assoc-in [env (exp 1)] (evaluate (exp 2) env))
    (= (exp 0) "if") (if (evaluate (exp 1) env) (evaluate (exp 2) env) (evaluate (exp 3) env))
    (= (exp 0) "quote") (exp 1)
    (= (exp 0) "lambda") (handle-lambda exp env)
    (fn? (env-find (exp 0) env)) ((env-find (exp 0) env) (map #(evaluate % env) (subvec exp 1)))
    :else ((evaluate (exp 0) env) (map #(evaluate % env) (subvec exp 1)))))

(defn parse [s eval?]
  (cond
    (re-find #"^\)" s) (throw-error)
    (and eval? (re-find #"^\(\s*(?:lambda|quote)\s+" s)) (let [[res rmn] (parse s false)] [(evaluate res :global) (trim rmn)])
    (re-find #"^\(" s) (loop [rst (trim (replace-first s #"^\(" "")), exp []]
                         (cond
                           (empty? rst) (throw-error)
                           (re-find #"^\)" rst) [(if eval? (evaluate exp :global) exp) (trim (replace-first rst #"^\)" ""))]
                           :else (when-let [[res rmn] (parse rst eval?)] (recur (trim rmn) (conj exp res)))))
    :else (if-let [res (re-find #"^\S*[^\(\)\s]+" s)] [(atomize (trim res)) (trim (subs s (count res)))] s)))

(defn -main [s] (parse (trim s) true))

;; quote
;; apply, begin, car, cdr, cons, eq, equal?, length, list, list?, map, null?, print

;(defn parse [s env parse-only?]
;  (cond
;    (re-find #"^\)" s) (throw-error)
;    (re-find #"^\(" s) (loop [rst (trim (replace-first s #"^\(" "")), exp []]
;                         (cond
;                           (empty? rst) (throw-error)
;                           (re-find #"^(?:lambda|quote)\s+" rst) (if parse-only?
;                                                                   (recur (replace-first rst #"^(?:lambda|quote)\s+" "") (conj exp (trim (re-find #"^(?:lambda|quote)\s+" rst))))
;                                                                   (let [[lexp rmn] (parse (str "(" rst) env true)] [lexp (trim rmn)]))
;                           (re-find #"^\)" rst) [(if parse-only? exp (evaluate exp env)) (trim (replace-first rst #"^\)" ""))]
;                           :else (when-let [[res rmn] (parse rst env parse-only?)] (recur (trim rmn) (conj exp res)))))
;    :else (if-let [res (re-find #"^\S*[^\(\)\s]+" s)] [(atomize (trim res)) (trim (subs s (count res)))] s)))

;(defn -main [s] (parse (trim s)))

;(defn parse
;  ([s cnt]
;   (cond
;     (re-find #"^\)" s) (throw-error)
;     (re-find #"^\(\s*(?:lambda|quote)\s+" s) (parse s (inc cnt) true)
;     (re-find #"^\(" s) (parse s 0 false)
;     :else (if-let [res (re-find #"^\S*[^\(\)\s]+" s)] [(atomize (trim res)) (trim (subs s (count res)))] s)))
;  ([s cnt parse-only?]
;   (loop [rst (trim (replace-first s #"^\(" "")), exp [], cnt]
;     (cond
;       (empty? rst) (throw-error)
;       (re-find #"^\)" rst) [(if parse-only? exp (evaluate exp :global)) (trim (replace-first rst #"^\)" ""))]
;       :else (when-let [[res rmn] (parse rst cnt parse-only?)] (recur (trim rmn) (conj exp res)))))))

;(defn parse [s env lm]
;  (cond
;    (re-find #"^\)" s) (throw-error)
;    (re-find #"^\(" s) (loop [rst (trim (replace-first s #"^\(" "")), exp []]
;                         (cond
;                           (empty? rst) (throw-error)
;                           (re-find #"^lambda\s+" rst) (if lm
;                                                         (recur (replace-first rst #"^lambda\s+" "") (conj exp "lambda"))
;                                                         (let [[lexp rmn] (parse (str "(" rst) env true)] [lexp (trim rmn)]))
;                           (re-find #"^\)" rst) [(if lm exp (evaluate exp env)) (trim (replace-first rst #"^\)" ""))]
;                           :else (when-let [[res rmn] (parse rst env lm)] (recur (trim rmn) (conj exp res)))))
;    :else (if-let [res (re-find #"^\S*[^\(\)\s]+" s)] [(atomize (trim res)) (trim (subs s (count res)))] s)))

;(defn parse [s parse-only?]
;  (cond
;    (re-find #"^\)" s) (throw-error)
;    (re-find #"^\(" s) (loop [rst (trim (replace-first s #"^\(" "")), exp []]
;                         (cond
;                           (empty? rst) (throw-error)
;                           (re-find #"^lambda\s+" rst) (if parse-only?
;                                                         (recur (replace-first rst #"^lambda\s+" "") (conj exp "lambda"))
;                                                         (let [[lexp rmn] (parse (str "(" rst) true)] [lexp (trim rmn)]))
;                           (re-find #"^quote\s+" rst) (let [[res rmn] (parse (replace-first rst #"^quote\s+" "") true)]
;                                                        [res (trim (replace-first rmn #"^\s*\)" ""))])
;                           (re-find #"^\)" rst) [(if parse-only? exp (evaluate exp :global)) (trim (replace-first rst #"^\)" ""))]
;                           :else (when-let [[res rmn] (parse rst parse-only?)] (recur (trim rmn) (conj exp res)))))
;    :else (if-let [res (re-find #"^\S*[^\(\)\s]+" s)] [(atomize (trim res)) (trim (subs s (count res)))] s)))