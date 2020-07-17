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
  ["GSY" "IAU" "PSR" "FXU" "SPY"])

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

(defn- get-tc-ndarrays [& syms]
  {:pre [(s/valid? (s/coll-of ::p-spec/sym) syms)]}
  (let [prices (map (comp :prices get-history) syms)
        xf-ind (comp (filter #(some? (:close %)))
                     (map :date))
        xf-ts      (comp (map #(sequence xf-ind %))
                         (map set))
        timestamps (sequence xf-ts prices)
        valid-timestamps (apply set/intersection timestamps)
        xf-sym (comp
                (filter #(some? (:close %)))
                (filter #(contains? valid-timestamps (:date %)))
                (map :close))
        xf-all (comp
                (map #(sequence xf-sym %))
                (map py/->numpy))]
   (sequence xf-all prices)))

(defn two-ticker-correlation
  "Get the correlation for two tickers"
  [sym-a sym-b]
  {:pre [(s/valid? ::p-spec/sym sym-a)
         (s/valid? ::p-spec/sym sym-b)]}
  (let [[a b] (get-tc-ndarrays sym-a sym-b)]
    (utils/py-get-in (np/corrcoef a b) [0 1])))

(defn ticker-correlations
  "Get the correlation for multiple tickers"
  [& syms]
  {:pre [(s/valid? (s/coll-of ::p-spec/sym) syms)]}
  (let [[a b] (apply get-tc-ndarrays syms)]
    (np/corrcoef a b)))

(defn rolling-ticker-correlation
  "Get the rolling correlation of two tickers.
  Give a `window` of time to roll over."
  [{:keys [sym-a sym-b window]
    :or {window 30}}]
  {:pre [(s/valid? ::p-spec/sym sym-a)
         (s/valid? ::p-spec/sym sym-b)
         (int? window)]}
  (let [[a b] (get-tc-ndarrays sym-a sym-b)]
    (-> a
        pd/Series
        (py. rolling window)
        (py. corr (pd/Series b))
        (py. dropna))))

(defn- -sharpe-ratio
  "determine the Sharpe ratio for an asset"
  [a b]
  {:pre [(= (py/python-type a) :ndarray)
         (= (py/python-type b) :ndarray)]}
  (op/truediv
   (np/mean (op/sub a b))
   (np/std (op/sub a b))))

(defn pct-change
  "calc % change for a time-series ndarray"
  [arr]
  {:pre [(= (py/python-type arr) :ndarray)]}
  (np/divide
   (np/diff arr)
   (py/get-item arr (pyb/slice nil (- 1)))))

(defn sharpe-ratio
  "determine the Sharpe ratio for an asset"
  [{:keys [sym-a sym-rf]}]
  {:pre [(s/valid? ::p-spec/sym sym-a)
         (s/valid? ::p-spec/sym sym-rf)]}
  (let [[asset riskfree]
        (map pct-change (get-tc-ndarrays sym-a sym-rf))]
    (-sharpe-ratio asset riskfree)))

(defn rolling-sharpe-ratio
  "Determine a rolling Sharpe Ratio for an asset"
  [{:keys [sym-a sym-rf window]
    :or {window 90}}]
  {:pre [(s/valid? ::p-spec/sym sym-a)
         (s/valid? ::p-spec/sym sym-rf)
         (int? window)]}
  (let [[asset riskfree]
        (map pct-change (get-tc-ndarrays sym-a sym-rf))]
    (py/->numpy
     (for [i (range window (py/len asset))]
       (-sharpe-ratio
        (py/get-item asset (pyb/slice (- i window) i))
        (py/get-item riskfree (pyb/slice (- i window) i)))))))

(comment
  (s/conform (s/coll-of ::p-spec/sym) (pyb/list ["SPY" "NFLX"]))
  (two-ticker-correlation "SPY" "NFLX")

  (py/get-item (np/array [1 1.1 1.21 1.331]) (pyb/slice nil (- 1)))
  (np/divide (np/array [1 2 3]) (np/array [2 4 6]))

  (rolling-sharpe-ratio {:sym-a "SPY" :sym-rf "BIL"})

  (plot/with-show "/tmp/temp.png"
    (plt/plot (rolling-sharpe-ratio
               {:sym-a "SPY"
                :sym-rf "BIL"
                :window 365})))

  (plot/with-show "/tmp/temp.png"
    (plt/plot (rolling-ticker-correlation
               {:sym-a "SPY"
                :sym-b "BIL"
                :window 365}))))
  {:pre [(s/valid? ::p-spec/sym symbol)]}
