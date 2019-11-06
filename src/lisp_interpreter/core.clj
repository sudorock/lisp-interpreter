(ns lisp-interpreter.core
  (:require [clojure.string :refer [split trim replace-first join escape]]))
(declare evaluate)

(defn throw-error [] (throw (Exception. "Parse Error")))

(def parent {})
(def macros {})
(def envs {:g {'+               #(apply + %)
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
               'list            vec
               'car             #(apply first %)
               'cdr             #(apply next %)
               'cons            (fn [a] (vec (apply #(cons %1 %2) a)))
               'concat          #(vec (apply concat %))
               'apply           #(apply %1 %&)
               'map             (fn [[f & bdy]] (map f (map vector (first bdy))))
               'print           #(apply prn %)
               'pi              3.141592653589793
               (symbol "true")  true
               (symbol "false") false}})

(defn env-find [sym env] (when-let [cur (envs env)] (if-let [vl (cur sym)] vl (env-find sym (parent env)))))

(defn atomize [el]
  (try (Integer/parseInt el) (catch Exception e (try (Double/parseDouble el) (catch Exception e (symbol el))))))

(defn get-arg-map [params args]
  (loop [rst-p params, rst-a args, result {}]
    (cond
      (empty? rst-p) result
      (= (first rst-p) '.) (conj result (hash-map (second rst-p) (vec rst-a)))
      :else (recur (rest rst-p) (rest rst-a) (conj result (hash-map (first rst-p) (first rst-a)))))))

(defn handle-lambda [exp env]
  (fn [args] (let [cur-env (keyword (gensym)) arg-map (get-arg-map (exp 1) args)]
               (def parent (assoc parent cur-env env))
               (def envs (assoc envs cur-env arg-map))
               (evaluate (exp 2) cur-env))))

(defn is-pair [exp] (and (vector? exp) (not-empty exp)))
(defn handle-quasi [exp]
  (if (is-pair exp)
    (cond
      (= (exp 0) 'unquote) (exp 1)
      (and (is-pair (exp 0)) (= ((get exp 0) 0) 'unquote-splice)) ['concat ((get exp 0) 1) (handle-quasi (subvec exp 1))]
      :else ['cons (handle-quasi (exp 0)) (handle-quasi (subvec exp 1))])
    ['quote exp]))

(defn evaluate [exp env]
  (cond
    ((some-fn number? boolean? fn? nil? #(contains? #{'if 'begin 'lambda 'define} %)) exp) exp
    (symbol? exp) (env-find exp env)
    (some #(= (get exp 0) %) ['set! 'define]) (def envs (assoc-in envs [env (exp 1)] (evaluate (exp 2) env)))
    (= (get exp 0) 'if) (if (evaluate (exp 1) env) (evaluate (exp 2) env) (evaluate (exp 3) env))
    (= (get exp 0) 'quote) (exp 1)
    (= (get exp 0) 'quasi-quote) (evaluate (handle-quasi (exp 1)) env)
    (= (get exp 0) 'begin) (last (map #(evaluate % env) (subvec exp 1)))
    (= (get exp 0) 'lambda) (handle-lambda exp env)
    (= (get exp 0) 'define-macro) (def macros (assoc macros (exp 1) (evaluate (exp 2) env)))
    (fn? (macros (get exp 0))) (evaluate ((macros (get exp 0)) (subvec exp 1)) env)
    (fn? (env-find (get exp 0) env)) ((env-find (get exp 0) env) (map #(evaluate % env) (subvec exp 1)))
    (fn? (get exp 0)) ((get exp 0) (map #(evaluate % env) (subvec exp 1)))
    :else (if (coll? exp) exp (throw-error))))

(defn parse [s eval?]
  (cond
    (re-find #"^\)" s) (throw-error)
    (and eval? (re-find #"^\(\s*(?:lambda|quote|quasi-quote|define-macro)\s+" s)) (let [[res rmn] (parse s false)] [(evaluate res :g) (trim rmn)])
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


;"(define-macro when (lambda (cnd then) (list (quote if) cnd then nil)))"
;"(define-macro when (lambda (cnd . then) (list (quote if) cnd then nil)))"
;"(define-macro when (lambda (cnd . then) (list (quote if) cnd (cons (quote begin) then)  nil)))"


;(define z (lambda (k l . els) (print k l els)))


;(define-macro unless (lambda (cnd . branch) (list (quote if) (list (quote not) cnd) (cons (quote begin) branch))))

;(define-macro my-or (lambda (x y) `(if ,x ,x ,y)))

;(define-macro my-or (lambda (x y) (quasi-quote (if (unquote x) (unquote x) (unquote y)))))