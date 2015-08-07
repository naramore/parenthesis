(ns parenthesis.core)

(defrecord Parenthesis [left-hand right-hand])
(def parenthesis (map (fn [[l r]] (Parenthesis. l r))
                      [[\[ \]]
                       [\( \)]
                       [\{ \}]]))

(defn parenthesis?
    ([p] (parenthesis? p :left-hand))
    ([p k] (->> (map #(= (k %) p) parenthesis)
                (some identity))))

(defn get-matching-parenthesis [p]
    (->> (filter #(= (:left-hand %) p) parenthesis)
         first
         :right-hand))

(defn matching-parenthesis? [p m]
    (= m (get-matching-parenthesis p)))

(defn verify [s]
    (loop [[f & coll] (seq s)
           stack []]
        (if f
            (if (parenthesis? f)
                (recur coll (conj stack f))
                (if (matching-parenthesis? (peek stack) f)
                    (recur coll (pop stack))
                    (if (parenthesis? f :right-hand)
                        false
                        (recur coll stack))))
            (zero? (count stack)))))