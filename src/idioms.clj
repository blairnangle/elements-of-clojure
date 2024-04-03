(ns idioms
  (:require [math :as math]
            [library :as library]))


;; Defaults for accumulating functions

(defn reducer
  ([f xs]
   (reduce f xs))
  ([f xs initial]
   (reduce f initial xs)))

(defn custom-concat
  "Wrapper around core library's concat fn to show that
  only the niladic and dyadic flavours are interesting
  —monadic returns the given argument, variadic is a
  reduction of the dyadic"
  ([] '())
  ([a] (seq a))
  ([a b] (concat a b))
  ([a b & rst]
   (reduce custom-concat (custom-concat a b) rst)))

(defn custom-conj
  "Wrapper around core library's conj fn to show that
  only the niladic and dyadic functions are interesting
  —monadic returns the given argument, variadic is a
  reduction of the dyadic"
  ([] [])
  ([a] a)
  ([a b] (conj a b))
  ([a b & rst]
   (reduce custom-conj (custom-conj a b) rst)))

(defn custom-cons
  "Wrapper around core library's cons fn to show that
  only the niladic and dyadic functions are interesting
  —monadic returns the given argument, variadic is a
  reduction of the dyadic"
  ([] [])
  ([a] a)
  ([a b] (cons a b))
  ([a b & rst]
   (reduce custom-cons (custom-cons a b) rst)))


;; Option maps over named parameters

(defn pi-positional
  "Calculates pi to `n` digits, with optional
  parameters for whether it should be done
  efficiently and correctly.

  Both default to `true`."

  ([n]
   (pi-positional n true))
  ([n efficiently?]
   (pi-positional n efficiently? true))
  ([n efficiently? correctly?]
   (cond
     (not correctly?)
     3.0
     (not efficiently?)
     (->>
       (repeatedly #(pi-positional n))
       (take 100)
       last)
     :else
     (math/pi-to-n-digits n)))
  )

(defn pi-option-map
  [n & {:keys [efficiently?
               correctly?]
        :or   {efficiently? true
               correctly?   true}
        :as   options}]
  (cond
    (not correctly?)
    3.0
    (not efficiently?)
    (->>
      (repeatedly #(pi-option-map n))
      (take 100)
      last)
    :else
    (->> options
         (apply concat)
         (apply math/pi-to-n-digits-with-options n))))

(defn pi-option-map-no-&
  [n {:keys [efficiently?
             correctly?]
      :or   {efficiently? true
             correctly?   true}
      :as   options}]
  (cond
    (not correctly?)
    3.0
    (not efficiently?)
    (->>
      (repeatedly #(pi-option-map n))
      (take 100)
      last)
    :else
    (math/pi-to-n-digits-with-options n options)))


;; Bindings

;; original

(defn- c [x]
  (library/compute x))

(defn- b [x]
  (c x))

(defn a [x]
  (b x))


;; after one refactor

(defn- c1 [x turbo-mode?]
  (library/compute1 x turbo-mode?))

(defn- b1 [x turbo-mode?]
  (c1 x turbo-mode?))

(defn a1 [x turbo-mode?]
  (b1 x turbo-mode?))


;; after two refactors

(defn a2
  ([x]
   (a2 x true))
  ([x turbo-mode?]
   (b1 x turbo-mode?)))


;; after three refactors

(def ^:dynamic *turbo-mode?* true)

(defmacro slowly [& body]
  `(binding [*turbo-mode?* false]
     ~@body))

(defn- c3 [x]
  (library/compute1 x *turbo-mode?*))

(defn- b3 [x]
  (c3 x))

(defn a3 [x]
  (b3 x))

(comment

  (let [values (range 33)]
    (slowly
      (let [values' (map a3 values)]
        (if (empty? values')
          (throw
            (IllegalArgumentException. "empty input"))
          values'))))
  ;; => (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 **34**)
  ;; the first chunk of 32 is realised within the binding within slowly (i.e., when *turbo-mode?* is false)
  ;; the last element is realised when *turbo-mode?* has its default value of true
  ;; this is because performing an empty? check on a chunked seq causes the first (up to) 32 elements to be realised

  (let [values (range 33)]
    (slowly
      (if (empty? values)
        (throw
          (IllegalArgumentException. "empty input"))
        (map a3 values))))
  ;; => (2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34)
  ;; i.e., the correct result
  ;; this is because we return a completely unrealised seq - we haven't performed an empty? check on the returned seq
  )


;; after four refactors

(defn c4 [x]
  (library/compute1 x *turbo-mode?*))

(defn b4 [x]
  (c4 x))

;; binding here, with very narrow scope, is much better than within a more general-purpose macro
;; here, we know that c won't be invoked lazily
;; here, binding can only be misused if someone changes the implementation of b or c
(defn a4
  ([x]
   (a4 b x))
  ([x turbo-mode?]
   (binding [*turbo-mode?* turbo-mode?]
     (b4 x))))


;; Favour atoms for mutable state

(defn transfer-refs! [account->balance a b amount]
  (dosync
    {a (-> account->balance
           (get a)
           (alter - amount))
     b (-> account->balance
           (get b)
           (alter + amount))}))

(defn transfer-atom! [account->balance a b amount]
  (-> account->balance
      (swap!
        #(-> %
             (update a - amount)
             (update b + amount)))
      (select-keys [a b])))

(def accounts-refs {:a (ref 100)
                    :b (ref 0)})

(def accounts-atom (atom {:a 100
                          :b 0}))

(comment

  ;; these both work, but the atom version should be preferred in almost all cases
  (transfer-refs! accounts-refs :a :b 10)
  (transfer-atom! accounts-atom :a :b 10)
  )


;; Communicate side effects consistently

(defn choose-moon []
  (rand-nth [:Europa :Ganymede :Io :Callisto :Amalthea :Kallichore]))

(defn fire-rocket! [moon]
  (println "I am a nil-returning, side-effecting fn! The value of moon is" moon))

(defn await-landing [moon]
  {:moon   moon
   :status :landed})

(comment

  (choose-moon)
  (fire-rocket! (choose-moon))

  ;; redundant do
  (let [moon (choose-moon)]
    (do
      (fire-rocket! moon)
      (await-landing moon)))

  ;; negative (white) space
  (let [moon (choose-moon)]

    (fire-rocket! moon)

    (await-landing moon))

  ;; bind return value of side-effecting fn to _
  (let [moon (choose-moon)
        _ (fire-rocket! moon)]
    (await-landing moon))
  )


;; Use data structure-specific functions where possible

(comment

  ;; a vector can be treated like a map of its indexes to its values
  (get [0 1] 1)
  (contains? [0 1] 2)

  ;; a map can be treated as a sequence of entry objects
  (map key {:a 1 :b 2})
  (map first {:a 1 :b 2})

  ;; we can even create lazy seqs from Java arrays
  (->> [1 2 3]
       int-array
       (map inc))

  ;; but we should use map-specific functions where possible
  (let [alpha->numeric {:a 1 :b 2}]
    ;(map first alpha->numeric)  no
    ;(map key alpha->numeric)  no
    (keys alpha->numeric))                                  ; yes
  )
