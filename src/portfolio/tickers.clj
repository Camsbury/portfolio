(ns portfolio.tickers
  (:require [base]
            [portfolio.utils :as utils]
            [portfolio.tickers :as tickers]
            [portfolio.plot :as plot]
            [clj-http.client :as client]
            [clj-time.core :as t]
            [environ.core :refer [env]]
            [clojure.set :as set]
            [taoensso.nippy :as nippy]
            [libpython-clj.python :refer [py. py.-]:as py]
            [libpython-clj.require :refer [require-python]]))

(require-python
 '[numpy :as np]
 '[pandas :as pd]
 '[matplotlib.pyplot :as plt])

(def TICKERS-OF-INTEREST
  ["GSY" "IAU" "PSR" "FXU" "SPY"])

(defn- get-store-path [symbol]
  (str "data/tickers/" symbol))

(defn store-history
  "Store history for a ticker from Yahoo Finance"
  [symbol]
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
  (when (not (utils/path-exists (get-store-path symbol)))
    (store-history symbol))
  (nippy/thaw-from-file (get-store-path symbol)))

(defn- tc-get-ndarrays [& syms]
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
  (let [[a b] (tc-get-ndarrays sym-a sym-b)]
    (utils/py-get-in (np/corrcoef a b) [0 1])))

(defn ticker-correlations
  "Get the correlation for multiple tickers"
  [syms]
  (let [[a b] (apply tc-get-ndarrays syms)]
    (np/corrcoef a b)))

(defn rolling-ticker-correlation
  "Plot the rolling correlation for two tickers to a file"
  [{:keys [sym-a sym-b window]
    :or {window 30}}]
  (let [[a b] (tc-get-ndarrays sym-a sym-b)]
    (-> a
        pd/Series
        (py. rolling window)
        (py. corr (pd/Series b))
        (py. dropna))))

(comment
  (two-ticker-correlation "SPY" "NFLX")

  (py.-
   (rolling-ticker-correlation
    {:sym-a "SPY"
     :sym-b "NFLX"})
   shape)


  (plot/with-show "/tmp/temp.png"
    (plt/plot (rolling-ticker-correlation
               {:sym-a "SPY"
                :sym-b "GLD"
                :window 365}))))
