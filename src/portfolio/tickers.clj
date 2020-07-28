(ns portfolio.tickers
  (:require [base]
            [portfolio.utils :as utils]
            [portfolio.plot :as plot]
            [portfolio.spec :as p-spec]
            [clojure.spec.alpha :as s]
            [clj-http.client :as client]
            [clj-time.core :as t]
            [environ.core :refer [env]]
            [clojure.set :as set]
            [taoensso.nippy :as nippy]
            [libpython-clj.python :refer [py. py.-]:as py]
            [libpython-clj.require :refer [require-python]]))

(require-python
 '[builtins          :as pyb]
 '[operator          :as op]
 '[numpy             :as np]
 '[pandas            :as pd]
 '[matplotlib.pyplot :as plt])

(def TICKERS-OF-INTEREST
  ["GSY" "IAU" "UDN" "PSR" "FXU" "SPY" "QQQ"])

(defn- get-store-path [symbol]
  {:pre [(s/valid? ::p-spec/sym symbol)]}
  (str "data/tickers/" symbol))

(defn store-history
  "Store history for a ticker from Yahoo Finance"
  [symbol]
  {:pre [(s/valid? ::p-spec/sym symbol)]}
  (->> (client/get "https://apidojo-yahoo-finance-v1.p.rapidapi.com/stock/v2/get-historical-data"
                   {:headers {"x-rapidapi-host"
                              "apidojo-yahoo-finance-v1.p.rapidapi.com"
                              "x-rapidapi-key"
                              (env :rapidapi-key)
                              "useQueryString"
                              true}
                    :query-params {"frequency" "1d"
                                   "filter" "history"
                                   "period1" (utils/to-epoch-timestamp (t/date-time 1970 1 1))
                                   "period2" (utils/to-epoch-timestamp (t/now))
                                   "symbol" symbol}})
       :body
       utils/from-json
       (utils/smart-freeze-to-file (get-store-path symbol))))


(defn get-history
  "Gets the history for a ticker.
  If it doesn't exist in storage, retrieves via Yahoo Finance"
  [symbol]
  {:pre [(s/valid? ::p-spec/sym symbol)]}
  (when (not (utils/path-exists (get-store-path symbol)))
    (store-history symbol))
  (nippy/thaw-from-file (get-store-path symbol)))

(defn get-ticker-df
  "Get the history of a ticker as a df"
  [symbol]
  {:pre [(s/valid? ::p-spec/sym symbol)]
   :post [(= (py/python-type %) :data-frame)]}
  (let [convert
        (fn [x]
          (if (= (py.- x name) "date")
            (pd/to_datetime x :unit "s")
            x))
        df (-> symbol
               get-history
               :prices
               pd/DataFrame
               (py. apply convert))]
    (py/set-attr! df "symbol" symbol)
    df))

(defn get-mergeable
  "Gets the mergeable form of the ticker data"
  [symbol]
  {:pre [(s/valid? ::p-spec/sym symbol)]
   :post [(= (py/python-type %) :data-frame)]}
  (-> symbol
      get-ticker-df
      (py. rename :columns {:adjclose symbol})
      (py/get-item (pyb/list (list "date" symbol)))
      (py. dropna)
      (py. set_index "date")))

(defn merge-symbols
  "Merges symbol data"
  [symbol & syms]
  {:pre [(s/valid? ::p-spec/sym symbol)
         (s/valid? (s/coll-of ::p-spec/sym) syms)]
   :post [(= (py/python-type %) :data-frame)]}
  (let [base   (get-mergeable symbol)
        rights (map get-mergeable syms)]
    (py. base join (pyb/list rights) :how "inner")))

(defn two-ticker-correlation
  "Get the correlation for two tickers"
  [sym-a sym-b]
  {:pre [(s/valid? ::p-spec/sym sym-a)
         (s/valid? ::p-spec/sym sym-b)]
   :post [(= (py/python-type %) :float)]}
  (-> (merge-symbols sym-a sym-b)
      (py. corr)
      (utils/py-get-in [sym-a sym-b])))

(defn ticker-correlations
  "Get the correlation for multiple tickers"
  [& syms]
  {:pre [(s/valid? (s/coll-of ::p-spec/sym) syms)]
   :post [(= (py/python-type %) :data-frame)]}
  (py. (apply merge-symbols syms) corr))

(defn rolling-ticker-correlation
  "Get the rolling correlation of two tickers.
  Give a `window` of time to roll over."
  [{:keys [sym-a sym-b window]
    :or {window 365}}]
  {:pre [(s/valid? ::p-spec/sym sym-a)
         (s/valid? ::p-spec/sym sym-b)
         (int? window)]}
  (let [merged (merge-symbols sym-a sym-b)]
    (-> (py/get-item merged sym-a)
        (py. rolling window)
        (py. corr (py/get-item merged sym-b))
        (py. dropna)
        (py. rename (str sym-a "/" sym-b)))))

(defn- -sharpe-ratio
  [a b]
  {:pre [(or
          (= (py/python-type a) :series)
          (= (py/python-type a) :ndarray))
         (or
          (= (py/python-type b) :series)
          (= (py/python-type b) :ndarray))]
   :post [(= (py/python-type %) :float)]}
  (op/truediv
   (np/mean (op/sub a b))
   (np/std (op/sub a b))))

(defn sharpe-ratio
  "determine the Sharpe ratio for an asset"
  [{:keys [sym-a sym-rf]}]
  {:pre [(s/valid? ::p-spec/sym sym-a)
         (s/valid? ::p-spec/sym sym-rf)]
   :post [(= (py/python-type %) :float)]}
  (let [merged (-> sym-a
                   (merge-symbols sym-rf)
                   (py. pct_change)
                   (py. dropna))]
    (-sharpe-ratio (py/get-item merged sym-a)
                   (py/get-item merged sym-rf))))

(defn rolling-sharpe-ratio
  "Determine a rolling Sharpe Ratio for an asset"
  [{:keys [sym-a sym-rf window]
    :or {window 90}}]
  {:pre [(s/valid? ::p-spec/sym sym-a)
         (s/valid? ::p-spec/sym sym-rf)
         (int? window)]}
  (let [merged (-> sym-a
                   (merge-symbols sym-rf)
                   (py. pct_change)
                   (py. dropna))]
    (-> (py/get-item merged sym-a)
        (py. rolling window)
        (py. apply #(-sharpe-ratio % (py/get-item merged sym-rf)))
        (py. dropna))))

(defn rolling-sharpe-ratio-np ; NOTE: currently much faster
  "Determine a rolling Sharpe Ratio for an asset using numpy"
  [{:keys [sym-a sym-rf window]
    :or {window 90}}]
  {:pre [(s/valid? ::p-spec/sym sym-a)
         (s/valid? ::p-spec/sym sym-rf)
         (int? window)]}
  (let [merged (-> sym-a
                   (merge-symbols sym-rf)
                   (py. pct_change)
                   (py. dropna))
        asset (py/get-item merged sym-a)
        riskfree (py/get-item merged sym-rf)]
    (py/->numpy
     (for [i (range window (py/len asset))]
       (-sharpe-ratio
        (py/get-item asset (pyb/slice (- i window) i))
        (py/get-item riskfree (pyb/slice (- i window) i)))))))

(comment
  (plot/with-show {:filename "tmp.png" :dpi 150}
    (let [c (rolling-ticker-correlation
             {:sym-a "GOOG"
              :sym-b "AAPL"
              :window 365})]
      (py. c plot :title (py.- c name))))

  (plot/with-show {:filename "tmp.png" :dpi 150}
    (-> "BIL"
        get-ticker-df
        (py. plot :x "date" :y "adjclose"))))
