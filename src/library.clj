(ns library)

(defn compute [x]
  (println "the value of x on the way in is" x)
  (+ x 1))

(defn compute1 [x turbo-mode?]
  ;(println "the value of x on the way in is" x)
  ;(println "the value of turbo-mode? on the way in is" turbo-mode?)
  (if turbo-mode?
    (+ x 2)
    (+ x 1)))
