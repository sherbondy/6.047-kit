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

(defn s-matrix [v w]
  (let [v-len (count v) 
        w-len (count w)]
    (apply vector 
      (for [i (range v-len)]
        (int-array
          (for [j (range w-len)]
            (if (or (= j 0) (= i 0))
              0
              neg-inf)))))))

(defn ppm [m]
  (print "[\n")
  (doseq [row m]
    (print (vec row) "\n"))
  (print "]\n"))


;; use get-in for easy access

(defn lcs 
  "Finds the longest common subsequence of two seqs v and w"
  [v w]
  (let [v-len (count v) w-len (count w)
        s (apply vector 
            (for [i (range v-len)]
              (int-array
                (for [j (range w-len)]
                  (if (or (= j 0) (= i 0))
                    0
                    neg-inf)))))]
    s))