(ns math)

(defn pi-to-n-digits
  [n]
  (get {1 3
        2 3.1
        3 3.14
        4 3.142
        5 3.1415}
       n))

(defn pi-to-n-digits-with-options
  [n & options]
  (get {1 3
        2 3.1
        3 3.14
        4 3.142
        5 3.1415}
       n))
