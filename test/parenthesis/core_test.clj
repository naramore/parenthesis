(ns parenthesis.core-test
  (:require [clojure.test :refer :all]
            [parenthesis.core :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.math.combinatorics :as combo]
            [clojure.test.check.clojure-test :refer :all]))

(def check-sum 400)

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
        (let [rand-p (->> parenthesis
                          (map vals)
                          rand-nth
                          vec)]
            parenthesis-gen rand-p inner-gen))
    ([[left-hand right-hand] inner-gen]
        (gen/tuple gen/string-alphanumeric
                   (gen/return left-hand)
                   (gen/vector inner-gen)
                   (gen/return right-hand)
                   gen/string-alphanumeric)))

(defn recursive-parenthesis-gen
    ([] (recursive-parenthesis-gen parenthesis-gen))
    ([p-gen]
        (->> (gen/recursive-gen p-gen
                                gen/string-alphanumeric)
             (gen/fmap #(->> (flatten %)
                             (apply str))))))

(defspec single-type-balanced-parenthesis
    check-sum
    (prop/for-all [p (recursive-parenthesis-gen (partial parenthesis-gen [\[ \]]))]
        (true? (verify p))))

(defspec single-type-unbalanced-parenthesis
    check-sum
    (prop/for-all [p (recursive-parenthesis-gen (partial parenthesis-gen [\[ \]]))]
        (let [invalid-p (remove-a-random-parenthesis p)
              result (verify invalid-p)]
            (if (empty? invalid-p)
                (true? result)
                (false? result)))))

;; these multiple parenthesis type checks use rand-nth and are therefore
;; not completely repeatable using the given generator seed
(defspec multiple-type-balanced-parenthesis
    check-sum
    (prop/for-all [p (recursive-parenthesis-gen)]
        (true? (verify p))))

(defspec multiple-type-unbalanced-parenthesis
    check-sum
    (prop/for-all [p (recursive-parenthesis-gen)]
        (let [invalid-p (remove-a-random-parenthesis p)
              result (verify invalid-p)]
            (if (empty? invalid-p)
                (true? result)
                (false? result)))))