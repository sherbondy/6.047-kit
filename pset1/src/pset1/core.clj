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
  (aset a i j v))

(defn getin [a [i j]]
  (aget a i j))

(defn s-matrix 
  "Initializes the score matrix with rows and columns set to 0"
  [rows cols]
  (let [s (make-array Long/TYPE rows cols)]   
    (doseq [r (range rows)
            c (range cols)]
      (seti s [r 0] 0)
      (seti s [0 c] 0))
    s))

(defn b-matrix 
  "Initializes the backtracing pointer matrix"
  [rows cols]
  (let [b (make-array clojure.lang.Keyword rows cols)]
    b))

;; The following is based on: https://github.com/hoeck/seq-diff
;; but I've gutted the procedure and plan to make it the abstract
;; basis for lcs with arbitrary scoring matrices.

;; could generalize this to support edit distance
(defn lcs
  "Returns the score of the lcs and the backtracing pointer matrix"
  [v w]
  (let [rows (inc (count v))
        cols (inc (count w))
        s (s-matrix rows cols)
        b (b-matrix rows cols)]
    (doseq [i (range 1 rows)]
      (doseq [j (range 1 cols)]
        (let [up   [(dec i) j]
              left [i (dec j)]
              diag [(dec i) (dec j)]]

          (seti s [i j]
            (if (= (v (dec i)) (w (dec j)))
              (inc (getin s diag))
              (apply max (map (partial getin s) 
                            [left
                             up]))))

          (seti b [i j]
            (condp = (getin s [i j])
              (getin s up)         :up
              (getin s left)       :left
              (inc (getin s diag)) :diag))
      )))

    [(getin s [(dec rows) (dec cols)])
     b]))

(defn lcs-vector 
  "Returns the actual longest common subsequence as a vector.
   Takes the backtracing pointer matrix and the 
   corresponding first vector argument (v) of lcs as input."
  [b v]
  (loop [i (- (count b) 2) 
         j (- (count (nth b 0)) 2) 
         m []]
    (if (or (= i 0) (= j 0)) m
      ; else
      (let [bij (getin b [i j])
            [ri rj]
            (if (= bij :diag) [(dec i) (dec j)]
              ;else
              (if (= bij :up) [(dec i) j]
                ;else
                [i (dec j)]))]

        (recur ri rj (if (= bij :diag) (cons (v i) m) m))
))))


