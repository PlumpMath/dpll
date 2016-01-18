(ns dpll.core
  (:require [clojure.math.combinatorics :as combo]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

;; TODO
;; - property based testing
;; - visualize fig 7.19
;; - stack overflow is a problem?

(defn fac
  ([n] (fac n 1))
  ([n res]
   (if (= n 1) res
       (recur (dec n) (* res n)))))

(defn seeded-rand-int [n seed]
  (let [r (java.util.Random. seed)]
    (.nextInt r n)))

(defn generate [num-clauses num-vars-clause num-vars seed]
  (let [vars (take num-vars [:A :B :C :D :E :F :G :H :I :J :K :L :M :N :O :P :Q :R :S :T :U :V :W :X :Y :Z])
        ps (take num-vars-clause (combo/nth-permutation vars (seeded-rand-int (if (<= num-vars 20)
                                                                                (dec (fac num-vars))
                                                                                Integer/MAX_VALUE)
                                                                              seed)))]
    (mapv (fn [_] (mapv #(rand-nth [% [:not %]]) ps))
          (range num-clauses))))


(defn write-cnf [cnf] ;; hack, better use clojure.walk
  (apply str (flatten
              (butlast
               (interleave
                (map (fn [c] (concat ["("]
                                    (butlast (interleave
                                              (map #(if (vector? %)
                                                      (str (name (second %)) "'") (name %)) c)
                                              (repeatedly (fn [] "|")))) [")"])) cnf)
                (repeatedly (fn [] "&")))))))


(defn read-cnf [s]
  (vec (for [and (re-seq #"[^&]+" s)
             :let [c (subs and 1 (dec (count and)))]]
         (vec (for [t (re-seq #"[^|]+" c)]
                (if (= (count t) 2)
                  [:not (keyword (subs t 0 1))]
                  (keyword t)))))))

(read-cnf "(A|B)&(B'|C|D')&(D|E')")

(let [cnf (generate 4 3 3 42)]
  (= cnf (read-cnf (write-cnf cnf))))


(defn all? [c]
  (reduce #(and (not (= :unresolved %2))
                %1 %2) true c))

(defn find-pure-symbol [symbols clauses]
  (let [clauses (set (apply concat clauses))]
    (first (for [s clauses
                 :let [neg-s (if (vector? s) (second s) [:not s])]
                 :when (and (symbols (if (vector? s) (second s) s)) (not (clauses neg-s)))]
             [(if (vector? s) (second s) s)
              (not (vector? s))]))))

(find-pure-symbol #{:A :B :C [:not :A] [:not :B]} [[:A [:not :B]] [[:not :A] [:not :B]]])

(defn assignment? [model t]
  (if (vector? t)
    (let [r (model (second t))]
      (if (nil? r) nil (not r)))
    (model t)))


(defn eval-clauses [clauses model]
  (let [res (map (fn [c] (map #(assignment? model %) c))
                 clauses)]
    (map (fn [r] (if (empty? (filter #(not (false? %)) r))
                  false
                  (if (some true? r)
                    true :unresolved))) res)))

(defn find-unit-clause [clauses model]
  (first (for [c clauses
               :let [[free & r] (filter #(= (model (if (vector? %) (second %) %)) nil) c)]
               :when (empty? r)]
           (if (vector? free)
             [(second free) false]
             [free true]))))

(find-unit-clause [[[:not :B] [:not :C] [:not :A]]
                   [:B [:not :C] :A]
                   [:B [:not :C] :A]]
                  {:A true, :B true})

(defn dpll [clauses symbols model]
  (let [res (eval-clauses clauses model)]
    (cond (all? res) model
          (some false? res) false
          :else (let [[p val] (find-pure-symbol symbols clauses)]
                  (if p
                    (dpll clauses (disj symbols p) (assoc model p val))
                    (let [[p val] (find-unit-clause clauses model)]
                      (if p
                        (dpll clauses (disj symbols p) (assoc model p val))
                        (if-let [[p] (seq symbols)]
                          (or (dpll clauses (disj symbols p) (assoc model p true))
                              (dpll clauses (disj symbols p) (assoc model p false)))))))))))


(defn dpll-satisfiable? [clauses]
  (dpll clauses (set (filter #(not= % :not) (flatten clauses))) {}))



(let [clauses [[[:not :B] [:not :C] [:not :A]] [:B [:not :C] :A] [:B [:not :C] :A]]
      symbols (set (filter #(not= % :not) (flatten clauses)))]
  (dpll clauses symbols {}))

(for [i (range 20)]
  (let [clauses (generate 100 5 5)
        symbols (set (filter #(not= % :not) (flatten clauses)))]
    (dpll clauses symbols {})))

(def sort-idempotent-prop
  (prop/for-all [v (gen/vector gen/int)]
    (= (sort v) (sort (sort v)))))

(def model-really-true
  (prop/for-all [i gen/int]
                (Let [clauses (generate 100 5 10 i)
                      model? (dpll-satisfiable? clauses)]
                  (if-not model? true
                          (= (eval-clauses clauses model?))))))


(tc/quick-check 100 model-really-true)
