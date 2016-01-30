(ns dpll.core
  (:require [clojure.math.combinatorics :as combo]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

;; TODO
;; - visualize fig 7.19
;; - stack overflow is a problem?

(defn set-jvm-seed [s]
  (let [f (.getDeclaredField java.lang.Math "randomNumberGenerator")]
    (.setAccessible f true)
    (.set f nil (java.util.Random. s))))

(defn fac
  ([n] (fac n 1N))
  ([n res]
   (if (or (= n 1) (= n 0)) res
       (recur (dec n) (* res n)))))

(defn rand-selection [n s]
  (loop [sel #{}]
    (if (= (count sel) n)
      (seq sel)
      (recur (conj sel (rand-nth s))))))

(defn generate [num-clauses num-vars-clause num-vars]
  (let [vars (take (max num-vars num-vars-clause)
                   [:A :B :C :D :E :F :G :H :I :J :K :L :M :N :O :P :Q :R :S :T :U :V :W :X :Y :Z
                    :a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v :w :x :y :z])]
    (mapv (fn [_] (mapv #(rand-nth [% [:not %]]) (rand-selection num-vars-clause vars)))
          (range num-clauses))))

(comment
  (do (set-jvm-seed 42)
      (generate 50 3 1 42)))

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

(let [cnf (generate 4 3 3)]
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

(def counter (atom 0))

(defn dpll [clauses symbols model]
  (swap! counter inc)
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


(comment

  (let [clauses [[[:not :B] [:not :C] [:not :A]] [:B [:not :C] :A] [:B [:not :C] :A]]
        symbols (set (filter #(not= % :not) (flatten clauses)))]
    (dpll clauses symbols {}))

  @counter

  (for [i (range 1 101)]
    (let [_ (set-jvm-seed 42)
          clauses (generate 50 3 i)]
      (reset! counter 0)
      [i (not (not (dpll-satisfiable? clauses))) @counter]))

  (def sort-idempotent-prop
    (prop/for-all [v (gen/vector gen/int)]
                  (= (sort v) (sort (sort v)))))

  (def model-really-true
    (prop/for-all [i gen/int]
                  (set-jvm-seed i)
                  (let [clauses (generate 100 5 10)
                        model? (dpll-satisfiable? clauses)]
                    (if-not model? true
                            (= (eval-clauses clauses model?))))))


  (tc/quick-check 100 model-really-true)
  )
