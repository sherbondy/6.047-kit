(ns pset1.core
  (:require [incanter.core :as i]))

(defn same-nuc-scores []
  (into {}
    (for [nuc [:A :T :C :G]]
      {nuc 1})))

(def vreverse (comp vec reverse))

(defn pp-nuc-scores []
  (into {}
    (for [pair [[:A :G] [:T :C]]]
      {pair -1/2 (vreverse pair) -1/2})))

(def nuc-scores 
  (merge (same-nuc-scores) (pp-nuc-scores)))

(def neg-inf Integer/MIN_VALUE)

(defn s-matrix [n m]
  (vec
    (for [i (range (inc n))]
      (vec
        (for [j (range (inc m))]
          (if (or (= j 0) (= i 0))
            0
            neg-inf))))))

(defn b-matrix [n m]
  (vec (repeat (inc n) (vec (repeat (inc m) nil)))))

(defn ppm [m]
  (print "[\n")
  (doseq [row m]
    (print row "\n"))
  (print "]\n"))


;; use get-in for easy access

(defn lcs 
  "Finds the longest common subsequence of two seqs v and w"
  [v w]
  (let [n (count v) m (count w)
        s (s-matrix n m)]
    s))