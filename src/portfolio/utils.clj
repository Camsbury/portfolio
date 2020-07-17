(ns portfolio.utils
  (:require [base]
            [camel-snake-kebab.core :as csk]
            [libpython-clj.python :as py]
            [clj-time.coerce :as tc]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [taoensso.nippy :as nippy]
            #_[libpython-clj.require :refer [require-python]]))

;; (require-python '[pandas :as pd]
;;                 '[kaggle.api :as kg])

(defn py-get-in [arr idxs]
  (reduce #(py/get-item %1 %2) arr idxs))

(defn to-epoch-timestamp [time]
  (-> time
      tc/to-long
      (quot 1000)
      str))

(defn from-epoch-timestamp [ts]
  (-> ts
      (* 1000)
      tc/from-long))

(defn from-json [raw]
  (json/read-str raw :key-fn (comp csk/->kebab-case keyword)))

(defn smart-freeze-to-file [path data]
  (when (not (.exists (io/as-file path)))
    (io/make-parents path))
  (nippy/freeze-to-file path data))

(defn path-exists [path]
  (.exists (io/as-file path)))

(defn collect-keys [keys maps]
  (reduce #(merge-with + %1 (select-keys %2 keys)) {} maps))
