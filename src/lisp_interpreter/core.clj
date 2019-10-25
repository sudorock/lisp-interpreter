(ns lisp-interpreter.core (:require [clojure.string :refer [split trim replace-first join escape]]))
(declare parse evaluate)

(defn throw-error [] (throw (Exception. "Parse Error")))
(defn uuid [] (str (java.util.UUID/randomUUID)))
(def const {"+"  #(apply + %), "-" #(apply - %), "*" #(apply * %), "/" #(apply / %), ">" #(apply > %), "<" #(apply < %),
            ">=" #(apply >= %), "<=" #(apply <= %), "="  #(apply = %),  "not" #(apply not %), "abs" (fn [a] (apply #(max % (- %)) a)),
            "max" #(apply max %), "min" #(apply min %), "sqrt" (fn [a] (apply #(Math/sqrt %) a)), "true" true, "false" false})

(def envs (atom {:global {}}))
(def parent (atom {}))

(defn env-find [v env] (when-let [cur (@envs env)] (if-let [vl (cur v)] vl (env-find v (@parent env)))))

(defn atomize [el]
  (try (Integer/parseInt el)
       (catch Exception e
         (try (Double/parseDouble el)
              (catch Exception e (if-let [cnst (get const el)] cnst el))))))

(defn handle-lambda [form env]
  (fn [args]
    (let [cur-env (uuid)]
      (swap! parent assoc cur-env env)
      (swap! envs assoc cur-env (zipmap (form 1) args))
      (evaluate (form 2) cur-env))))

(defn evaluate [form env]
  (cond
    (or (boolean? form) (number? form)) form
    (string? form) (env-find form env)
    (= (form 0) "define") (swap! envs assoc-in [env (form 1)] (evaluate (form 2) env))
    (= (form 0) "if") (if (evaluate (form 1) env) (evaluate (form 2) env) (evaluate (form 3) env))
    (= (form 0) "lambda") (handle-lambda form env)
    (fn? (env-find (form 0) env)) ((env-find (form 0) env) (map #(evaluate % env) (subvec form 1)))
    :else ((form 0) (map #(evaluate % env) (subvec form 1)))))

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