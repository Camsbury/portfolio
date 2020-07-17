(ns base
  (:require [environ.core :refer [env]]
            [libpython-clj.python :as py]
            [clojure.string :refer [index-of]]))

;;; Initialize python env
(let [python-path (env :python-path)
      full-version (env :python-version)
      python-version (->> (index-of full-version ".")
                          inc
                          (index-of full-version ".")
                          (subs full-version 0))]
  (py/initialize!
   :python-executable (str python-path "/bin/python" python-version)
   :library-path (str python-path "/lib/libpython" python-version "m.dylib")))
