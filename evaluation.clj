;; gorilla-repl.fileformat = 1

;; **
;;; # DPLL Evaluation
;; **

;; @@
(ns parched-firefly
  (:require [gorilla-plot.core :as plot]
            [dpll.core :refer [generate dpll-satisfiable? set-jvm-seed]]
            [clojure.core.matrix :as mat]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(generate 3 2 2)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:A</span>","value":":A"},{"type":"html","content":"<span class='clj-keyword'>:B</span>","value":":B"}],"value":"[:A :B]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:not</span>","value":":not"},{"type":"html","content":"<span class='clj-keyword'>:B</span>","value":":B"}],"value":"[:not :B]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:not</span>","value":":not"},{"type":"html","content":"<span class='clj-keyword'>:A</span>","value":":A"}],"value":"[:not :A]"}],"value":"[[:not :B] [:not :A]]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:not</span>","value":":not"},{"type":"html","content":"<span class='clj-keyword'>:A</span>","value":":A"}],"value":"[:not :A]"},{"type":"html","content":"<span class='clj-keyword'>:B</span>","value":":B"}],"value":"[[:not :A] :B]"}],"value":"[[:A :B] [[:not :B] [:not :A]] [[:not :A] :B]]"}
;; <=

;; @@
(dpll-satisfiable? [[:A :B] [[:not :B] [:not :A]] [[:not :A] :B]])
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:A</span>","value":":A"},{"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"}],"value":"[:A false]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:B</span>","value":":B"},{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}],"value":"[:B true]"}],"value":"{:A false, :B true}"}
;; <=

;; @@
(def sat-prob 
 (let [runs 10
       num-clauses 50]
  (for [i (range 6 50)]
    [(/ num-clauses i)
     (/ (count (for [seed (range runs)
      	         :let [_ (set-jvm-seed seed)
                       clauses (generate num-clauses 3 i)]
                 :when (dpll-satisfiable? clauses)]
                seed))
       runs)])))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;parched-firefly/sat-prob</span>","value":"#'parched-firefly/sat-prob"}
;; <=

;; @@
(plot/list-plot sat-prob :joined true :plot-range [[0 8] :all])
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":50,"bottom":20,"right":10},"data":[{"name":"741be927-7cf6-4d83-bd6b-f938059fa716","values":[{"x":8.333333333333332,"y":0},{"x":7.142857142857143,"y":0},{"x":6.25,"y":0.2},{"x":5.555555555555556,"y":0.2},{"x":5,"y":0.1},{"x":4.545454545454545,"y":0.4},{"x":4.166666666666667,"y":0.5},{"x":3.846153846153846,"y":0.5},{"x":3.571428571428571,"y":0.7},{"x":3.333333333333333,"y":0.6},{"x":3.125,"y":0.7},{"x":2.941176470588235,"y":0.9},{"x":2.777777777777778,"y":0.8},{"x":2.631578947368421,"y":0.9},{"x":2.5,"y":0.9},{"x":2.380952380952381,"y":1},{"x":2.272727272727273,"y":1},{"x":2.173913043478261,"y":0.9},{"x":2.083333333333333,"y":0.9},{"x":2,"y":1},{"x":1.923076923076923,"y":1},{"x":1.851851851851852,"y":1},{"x":1.785714285714286,"y":1},{"x":1.724137931034483,"y":1},{"x":1.666666666666667,"y":1},{"x":1.612903225806452,"y":1},{"x":1.5625,"y":0.9},{"x":1.515151515151515,"y":1},{"x":1.470588235294118,"y":1},{"x":1.428571428571429,"y":1},{"x":1.388888888888889,"y":1},{"x":1.351351351351351,"y":0.9},{"x":1.315789473684211,"y":1},{"x":1.282051282051282,"y":1},{"x":1.25,"y":1},{"x":1.219512195121951,"y":1},{"x":1.19047619047619,"y":1},{"x":1.162790697674419,"y":1},{"x":1.136363636363636,"y":1},{"x":1.111111111111111,"y":1},{"x":1.08695652173913,"y":1},{"x":1.063829787234043,"y":1},{"x":1.041666666666667,"y":1},{"x":1.020408163265306,"y":1}]}],"marks":[{"type":"line","from":{"data":"741be927-7cf6-4d83-bd6b-f938059fa716"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[0,8]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"741be927-7cf6-4d83-bd6b-f938059fa716","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 50, :bottom 20, :right 10}, :data [{:name \"741be927-7cf6-4d83-bd6b-f938059fa716\", :values ({:x 25/3, :y 0} {:x 50/7, :y 0} {:x 25/4, :y 1/5} {:x 50/9, :y 1/5} {:x 5, :y 1/10} {:x 50/11, :y 2/5} {:x 25/6, :y 1/2} {:x 50/13, :y 1/2} {:x 25/7, :y 7/10} {:x 10/3, :y 3/5} {:x 25/8, :y 7/10} {:x 50/17, :y 9/10} {:x 25/9, :y 4/5} {:x 50/19, :y 9/10} {:x 5/2, :y 9/10} {:x 50/21, :y 1} {:x 25/11, :y 1} {:x 50/23, :y 9/10} {:x 25/12, :y 9/10} {:x 2, :y 1} {:x 25/13, :y 1} {:x 50/27, :y 1} {:x 25/14, :y 1} {:x 50/29, :y 1} {:x 5/3, :y 1} {:x 50/31, :y 1} {:x 25/16, :y 9/10} {:x 50/33, :y 1} {:x 25/17, :y 1} {:x 10/7, :y 1} {:x 25/18, :y 1} {:x 50/37, :y 9/10} {:x 25/19, :y 1} {:x 50/39, :y 1} {:x 5/4, :y 1} {:x 50/41, :y 1} {:x 25/21, :y 1} {:x 50/43, :y 1} {:x 25/22, :y 1} {:x 10/9, :y 1} {:x 25/23, :y 1} {:x 50/47, :y 1} {:x 25/24, :y 1} {:x 50/49, :y 1})}], :marks [{:type \"line\", :from {:data \"741be927-7cf6-4d83-bd6b-f938059fa716\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [0 8]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"741be927-7cf6-4d83-bd6b-f938059fa716\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; @@

;; @@
