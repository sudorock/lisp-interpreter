(ns lisp-interpreter.practice-macros)

(defmacro unless [cnd & forms]
  `(if (not ~cnd) ~@forms))

