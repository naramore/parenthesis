(ns parenthesis.core-test
  (:require [clojure.test :refer :all]
            [parenthesis.core :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.math.combinatorics :as combo]
            [clojure.test.check.clojure-test :refer :all]))

(def check-sum 100)

(defn remove-nth [s n]
    (if (and ((complement nil?) n)
             (<= 0 n (dec (count s))))
        (->> (vec s)
             (#(concat (subvec % 0 n)
                       (subvec % (inc n) (count s))))
             (apply str))
        nil))

(defn remove-a-random-parenthesis
    ([s] (remove-a-random-parenthesis s 10000))
    ([s limit]
        (if (zero? (count s))
            nil
            (->> (map rand-int (repeat limit (count s)))
                 (map (juxt identity (partial nth s)))
                 (filter #((->> parenthesis
                              (mapcat vals)
                              set)
                           (second %)))
                 ffirst
                 (remove-nth s)))))

(defn parenthesis-gen
    ([inner-gen]
        (parenthesis-gen [\( \)] inner-gen))
    ([[left-hand right-hand] inner-gen]
        (gen/tuple gen/string-alphanumeric
                   (gen/return left-hand)
                   (gen/vector inner-gen)
                   (gen/return right-hand)
                   gen/string-alphanumeric)))

(defn recursive-parenthesis-gen [p-type]
    (->> (gen/recursive-gen (partial parenthesis-gen p-type)
                            gen/string-alphanumeric)
         (gen/fmap #(->> (flatten %)
                         (apply str)))))

(defspec single-type-balanced-parenthesis
    check-sum
    (prop/for-all [p (recursive-parenthesis-gen [\[ \]])]
        (true? (verify p))))

(defspec single-type-unbalanced-parenthesis
    check-sum
    (prop/for-all [p (recursive-parenthesis-gen [\[ \]])]
        (let [invalid-p (remove-a-random-parenthesis p)]
            (false? (verify invalid-p)))))