(ns toolbelt.macros
  (:require [clojure.test :refer [is]]))

(defmacro is-not
  [expr]
  `(is (not ~expr)))
