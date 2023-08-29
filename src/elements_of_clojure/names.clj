(ns elements-of-clojure.names)

(defn get-sol-jupiter
  "Does a deep lookup of key `k` within `m` under
  `:sol` and `:jupiter`, returning `not-found` or
  `nil` if no such key exist"
  ([m k]
   (get-sol-jupiter m k nil))
  ([m k not-found]
   (get-in m [:sol :jupiter k] not-found)))

(defn get-jovian-moon
  "Looks up a moon of Jupiter, returning `not-found`
  or `nil` if no such key exist"
  ([galaxy moon]
   (get-jovian-moon galaxy moon nil))
  ([m k not-found]
   (get-in m [:sol :jupiter k] not-found)))

(def errors #{:too-hot :too-cold})

(def porridge-errors #{:too-hot :too-cold})

(def bed-errors #{:too-hard :too-soft})

(macroexpand get-sol-jupiter)
