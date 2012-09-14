(ns pset1.core)

(def nuc-scores (merge (same-nuc-scores) (pp-nuc-scores)))

(defn pp-nuc-scores []
  (into {} (for [pair [[:A :G] [:T :C]]]
             {pair -1/2 (vreverse pair) -1/2})))

(nuc-scores [:A :T] -1) ;; default to -1 for undefined scores