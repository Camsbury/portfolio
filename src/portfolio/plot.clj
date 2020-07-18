(ns portfolio.plot
  (:require [libpython-clj.require :refer [require-python]]
            [libpython-clj.python :as py :refer [py.]]))


(def mplt (py/import-module "matplotlib"))
(py. mplt "use" "Agg")

(require-python 'matplotlib.pyplot)
(require-python 'matplotlib.backends.backend_agg)
(require-python 'numpy)


(defmacro with-show
  "Takes forms with mathplotlib.pyplot to then show locally"
  [{:keys [filename dpi]
    :or {filename "/tmp/tmp.png"
         dpi      100}}
   & body]
  `(let [_# (matplotlib.pyplot/clf)
         fig# (matplotlib.pyplot/figure)
         agg-canvas# (matplotlib.backends.backend_agg/FigureCanvasAgg fig#)]
     ~(cons 'do body)
     (py. agg-canvas# "draw")
     (matplotlib.pyplot/savefig ~filename :dpi ~dpi)))
