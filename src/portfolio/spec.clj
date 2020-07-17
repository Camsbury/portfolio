(ns portfolio.spec
  (:require [base]
            [clojure.spec.alpha :as s]))

(s/def ::sym (s/and
              string?
              #(re-matches #"^[a-zA-Z]*$" %)))

