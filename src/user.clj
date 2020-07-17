(ns user
  (:require [wing.repl]))

(comment
  (wing.repl/sync-libs!) ; refresh your deps - doesn't remove from classpath
  (wing.repl/add-lib! 'ring {:mvn/version "1.8.1"}) ; dynamically add lib
  )
