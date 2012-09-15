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



(defn ppm [m]
  (print "[\n")
  (doseq [row m]
    (print (vec row) "\n"))
  (print "]\n"))

(defn seti [a [i j] v]
  (aset a (int i) (int j) (int v)))

(defn getin [a [i j]]
  (aget a i j))

(defn s-matrix [rows cols]
  (let [s (make-array Integer/TYPE rows cols)]   
    (doseq [r (range rows)
            c (range cols)]
      (seti s [r 0] 0)
      (seti s [0 c] 0))
    s))

(defn b-matrix [rows cols]
  (let [b (make-array clojure.lang.Keyword rows cols)]))

(defn lcs-matrix
  "Builds and fills a matrix to find the longest common subsequence
  (LCS) of two sequences s and t.
  Both must be Indexed (supporting the nth method).
  Returns a twodimensional array of Integers"
  [v w]
  (let [rows (inc (count v))
        cols (inc (count w))
        s (s-matrix rows cols)]
    (doseq [i (range 1 rows)]
      (doseq [j (range 1 cols)]
        (seti s [i j]
          (if (= (v (dec i)) (w (dec j)))
            (inc (getin s [(dec i) (dec j)]))
            (apply max (map (partial getin s) 
                          [[i (dec j)]
                          [(dec i) j]]))))))
    s))

(defn lcs-vec
  "Given a lcs-matrix (created with the lcs-matrix function) and the
  corresponding sequences, returns the actual lcs as a vector."
  [lcs-matrix s t & {:keys [compare] :or {compare =}}]
  (let [m lcs-matrix
        f (fn lcs-vec-rec [i j]
            (cond (or (= 0 i) (= 0 j))
                  []
                  (compare (nth s (dec i)) (nth t (dec j)))
                  (conj (lcs-vec-rec (dec i) (dec j)) (nth s (dec i)))
                  (< (aget m i (dec j)) (aget m (dec i) j))
                  (lcs-vec-rec (dec i) j)
                  :else
                  (lcs-vec-rec i (dec j))))]
    (f (count s) (count t))))


