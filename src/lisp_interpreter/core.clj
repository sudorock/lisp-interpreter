(ns lisp-interpreter.core
  (:require [clojure.string :refer [split trim replace-first join escape]]))
(declare parse evaluate)

(defn throw-error [] (throw (Exception. "Parse Error")))
(defn uuid [] (str (java.util.UUID/randomUUID)))

(def envs (atom {:global {}}))
(def parent (atom {}))
(def const {"+"   #(apply + %), "-" #(apply - %), "*" #(apply * %), "/" #(apply / %), ">" #(apply > %), "<" #(apply < %),
            ">="  #(apply >= %), "<=" #(apply <= %), "=" #(apply = %), "not" #(apply not %), "max" #(apply max %),
            "min" #(apply min %), "sqrt" (fn [a] (apply #(Math/sqrt %) a)), "true" true, "false" false,
            "abs" (fn [a] (apply #(max % (- %)) a)), "pi" 3.141592653589793})

(defn env-find [v env] (when-let [cur (@envs env)] (if-let [vl (cur v)] vl (env-find v (@parent env)))))

(defn atomize [el]
  (try (Integer/parseInt el)
       (catch Exception e (try (Double/parseDouble el)
                               (catch Exception e (if-let [cnst (get const el)] cnst el))))))

(defn handle-lambda [exp env]
  (fn [args]
    (let [cur-env (uuid)]
      (println exp)
      (println args)
      (swap! parent assoc cur-env env)
      (swap! envs assoc cur-env (zipmap (exp 1) args))
      (evaluate (exp 2) cur-env))))

(defn evaluate [exp env]
  (cond
    (or (boolean? exp) (number? exp) (fn? exp)) exp
    (string? exp) (env-find exp env)
    (= (exp 0) "define") (swap! envs assoc-in [env (exp 1)] (evaluate (exp 2) env))
    (= (exp 0) "if") (if (evaluate (exp 1) env) (evaluate (exp 2) env) (evaluate (exp 3) env))
    (= (exp 0) "lambda") (handle-lambda exp env)
    (fn? (env-find (exp 0) env)) ((env-find (exp 0) env) (map #(evaluate % env) (subvec exp 1)))
    :else ((exp 0) (map #(evaluate % env) (subvec exp 1)))))

(defn parse [s env lmda]
  (cond
    (re-find #"^\)" s) (throw-error)
    (re-find #"^\(" s) (loop [rst (trim (replace-first s #"^\(" "")), exp []]
                         (cond
                           (empty? rst) (throw-error)
                           (re-find #"^lambda\s+" rst) (if lmda
                                                         (recur (replace-first rst #"^lambda\s+" "") (conj exp "lambda"))
                                                         (let [[lexp rmn] (parse (str "(" rst) env true)] [lexp (trim rmn)]))
                           (re-find #"^\)" rst) [(if lmda exp (evaluate exp env)) (trim (replace-first rst #"^\)" ""))]
                           :else (when-let [[res rmn] (parse rst env lmda)] (recur (trim rmn) (conj exp res)))))
    :else (if-let [res (re-find #"^\S*[^\(\)\s]+" s)] [(atomize (trim res)) (trim (subs s (count res)))] s)))

(defn -main [s] (parse (trim s) :global false))