(ns lisp-interpreter.cond-let)

;(defmacro cond-let [bnd exp & args]
;  `(if (= ~bnd :else)
;     exp
;     (if-let bnd
;       exp
;       (if args
;         (cond-let ~@args)
;         nil))))

;(defmacro cond-let [& args]
;  `(println ~(first args) ~(second args) ~@args))

(defmacro or-1 [& args]
  `(let [cnd# ~(first args)]
    (if (empty? ~args) nil (if cnd# cnd# (or-1 ~@(rest args))))))