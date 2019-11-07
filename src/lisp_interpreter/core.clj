(ns lisp-interpreter.core
  (:require [clojure.string :refer [split trim replace-first join escape]]
            [lisp-interpreter.cond-let :refer [cond-let]]))
(declare evaluate reader -main)

(defn throw-error [s] (.println *err* (format "ERROR: %s" s)))

(def parent {})
(def macros {})
(def envs {:g {'+ #(apply + %) '- #(apply - %) '* #(apply * %) '/ #(apply / %) '>  #(apply > %) '<  #(apply < %)
               '>= #(apply >= %) '<= #(apply <= %) '=  #(apply = %) 'not  #(apply not %) 'max  #(apply max %)
               'min  #(apply min %) 'sqrt (fn [a] (apply #(Math/sqrt %) a)) 'expt (fn [a] (apply #(Math/pow %1 %2) a))
               'round (fn [a] (apply #(Math/round %) a)) 'abs (fn [a] (apply #(max % (- %)) a)) 'number? #(apply number? %)
               'procedure? #(apply fn? %) 'symbol?  #(apply string? %) 'equal? #(apply = %) 'list vec 'car #(apply first %)
               'cdr #(apply next %) 'cons (fn [a] (vec (apply #(cons %1 %2) a))) 'concat  #(vec (apply concat %))
               'apply  #(apply %1 %&) 'map (fn [[f & bdy]] (map f (map vector (first bdy))))'print  #(apply prn %)
               'pi 3.141592653589793 (symbol "true") true (symbol "false") false}})

(defn env-find [sym env] (when-let [cur (envs env)] (if-let [vl (cur sym)] vl (env-find sym (parent env)))))

(defn atomize [el]
  (if (re-find #"^\".*\"$" el)
    (clojure.string/replace el #"^\"|\"$" "")
    (try (Integer/parseInt el) (catch Exception e (try (Double/parseDouble el) (catch Exception e (symbol el)))))))

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

(defn handle-quasi [exp]
  (letfn [(is-pair [exp] (and (vector? exp) (not-empty exp)))]
    (if (is-pair exp)
      (cond
        (= (exp 0) 'unqt) (exp 1)
        (and (is-pair (exp 0)) (= ((get exp 0) 0) 'unqt-splice)) ['concat ((get exp 0) 1) (handle-quasi (subvec exp 1))]
        :else ['cons (handle-quasi (exp 0)) (handle-quasi (subvec exp 1))])
      ['quote exp])))

(defn apply-fn [func arg-list env]
  (try (func (map #(evaluate % env) arg-list))
       (catch Exception e (throw-error "Invalid Form"))))

(defn evaluate
  ([exp] (evaluate exp :g))
  ([exp env] (if-let [op (and (vector? exp) (if (vector? (get exp 0)) (evaluate (get exp 0) env) (get exp 0)) )]
               (cond-let
                 (some #(= op %) ['set! 'define]) (def envs (assoc-in envs [env (exp 1)] (evaluate (exp 2) env)))
                 (= op 'if) (if (evaluate (exp 1) env) (evaluate (exp 2) env) (evaluate (get exp 3) env))
                 (= op 'quote) (exp 1)
                 (= op 'q-quote) (evaluate (handle-quasi (exp 1)) env)
                 (= op 'begin) (last (map #(evaluate % env) (subvec exp 1)))
                 (= op 'lambda) (handle-lambda exp env)
                 (= op 'def-macro) (def macros (assoc macros (exp 1) (evaluate (exp 2) env)))
                 [macro (macros op)] (evaluate (macro (subvec exp 1)) env)
                 [func (env-find op env)] (apply-fn func (subvec exp 1) env)
                 :else (apply-fn op (subvec exp 1) env))
               (cond-let
                 [value (env-find exp env)] value
                 ((some-fn number? boolean? fn? nil? string?) exp) exp
                 :else (throw-error (format "Unable to resolve symbol: %s" exp))))))

(defn reader-macro [s]
  (cond
    (re-find #"^'\S+" s) (let [[res rmn] (reader (subs s 1))] [['quote res] rmn])
    (re-find #"^`\S+" s) (let [[res rmn] (reader (subs s 1))] [['q-quote res] rmn])
    (re-find #"^~@\S+" s) (let [[res rmn] (reader (subs s 2))] [['unqt-splice res] rmn])
    (re-find #"^~\S+" s) (let [[res rmn] (reader (subs s 1))] [['unqt res] rmn])
    :else nil))

(defn reader [s]
  (cond-let
    (re-find #"^\)" s) (throw-error "Unmatched delimiter")
    (re-find #"^\(" s) (loop [rst (trim (replace-first s #"^\(" "")), exp []]
                         (cond
                           (empty? rst) (throw-error "EOF while reading")
                           (re-find #"^\)" rst) [exp (trim (replace-first rst #"^\)" ""))]
                           :else (when-let [[res rmn] (reader rst)] (recur (trim rmn) (conj exp res)))))
    [expanded (reader-macro s)] expanded
    :else (if-let [res (re-find #"^\S*[^\(\)\s]+" s)] [(atomize (trim res)) (trim (subs s (count res)))] s)))

;; Interpreter and REPL
(defn interpret [s]
  (let [[ast rmn] (reader (trim s))]
    (if (empty? rmn)
      (evaluate ast :g)
      (throw-error "Unmatched delimiter"))))

(defn repl []
  (do (printf "\033[0;1mcljisp ~ \u03BB  \033[34;1m") (flush)
      (let [res (interpret (read-line))] (printf "%s\n" (pr-str res)) (repl))))

(defn -main [] (repl))





;"(def-macro when (lambda (k . l) (q-quote (if (unqt k) (begin (unqt-splice l)) ))))"

;"(def-macro when (lambda (k . l) `(if ~k (begin ~@l))))"



;(defn evaluate
;  ([exp] (evaluate exp :g))
;  ([exp env] (if-let [op (and (vector? exp) (if (vector? (get exp 0)) (evaluate (get exp 0) :g) (get exp 0)) )]
;               (cond
;                 (some #(= op %) ['set! 'define]) (def envs (assoc-in envs [env (exp 1)] (evaluate (exp 2) env)))
;                 (= op 'if) (if (evaluate (exp 1) env) (evaluate (exp 2) env) (evaluate (get exp 3) env))
;                 (= op 'quote) (exp 1)
;                 (= op 'q-quote) (evaluate (handle-quasi (exp 1)) env)
;                 (= op 'begin) (last (map #(evaluate % env) (subvec exp 1)))
;                 (= op 'lambda) (handle-lambda exp env)
;                 (= op 'def-macro) (def macros (assoc macros (exp 1) (evaluate (exp 2) env)))
;                 (fn? (macros op)) (evaluate ((macros op) (subvec exp 1)) env)
;                 (fn? (env-find op env)) ((env-find op env) (map #(evaluate % env) (subvec exp 1)))
;                 (fn? op) (op (map #(evaluate % env) (subvec exp 1)))
;                 :else (throw-error "Invalid Expression"))
;               (if-let [value (env-find exp env)]
;                 value
;                 (if ((some-fn number? boolean? fn? nil? string?) exp)
;                   exp
;                   (format "Unable to resolve symbol: %s" exp))))))

