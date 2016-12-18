(ns series.core)

(defn neg-complement
  "Takes a fn f and returns a fn that takes the same arguments as f,
  has the same effects, if any, and returns the opposite numeric value. based on complement builtin"
  {:added "1.0"
   :static true}
  [f] 
  (fn 
    ([] (*' -1 (f)))
    ([x] (*' -1 (f x)))
    ([x y] (*' -1 (f x y)))
    ([x y & zs] (*' -1 (apply f x y zs)))))

(defn dif-series [ init func rate ]
  (mapcat #(take rate %1) (iterate func init)))

(defn q-series [qq func]
  (lazy-seq (let [xx (func qq) ]
		 (cons (rest xx) (q-series (first xx) func)))))

(defn step-series [aa] (let [bb (peek aa) cc (pop aa)] (list (conj cc (+ bb (peek cc)) (peek cc)) bb)))

(defn st-b [aa]
  (let [ minus-2 (take-nth (st-b aa) 1) minus-1 (take-nth (rest (st-b aa)) 1) ]
       (flatten
	(list
	 aa
	 (+ (first minus-2) (first minus-1))
	 (first minus-1)))))

(defn s-b [len counter aa]
  (if (< len (count aa))
      aa
    (lazy-seq
     (s-b len (inc counter)
	  (flatten
	   (list
	    aa
	    (+ (nth aa (+ 0 counter)) (nth aa (+ 1 counter)))
	    (nth aa (+ 1 counter))))))))

(defn stern-brocot [aa]
  (flatten (list (rest aa) (+ (first aa) (nth aa 1)) (nth aa 1))))

(defn queue
      ([] clojure.lang.PersistentQueue/EMPTY)
      ([coll] (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))
;; per http://stackoverflow.com/questions/3136453/immutable-queue-in-clojure

(defn fib [aa]
  (list (first (rest aa)) (+ (first aa) (first (rest aa)))))
(defn nat-fib [aa]
  (list (nth aa 1) (nth aa 2) (+ (first aa) (nth aa 2))))

; fibonacci found in nature (dif-series '(1 1 1) nat-fib 1)
; fibonacci (dif-series '(1 1) fib 1)
; lucas numbers (dif-series '(2 1) fib 1)
; brady numbers https://www.youtube.com/watch?v=D8ntDpBm6Ok (dif-series '(2308 4261) fib 1) 

(defn app-func [xx] (fn [yy] (apply xx yy)))
(defn multi-pop [ qq iter-num ] (last (take (+ 1 iter-num) (iterate pop qq))))
(defn multi-rest [ ss iter-num ] (last (take (+ 1 iter-num) (iterate rest ss))))
(defn enough-state? [dif-step-arg]
      (if (>= (count (nth dif-step-arg 2)) (nth dif-step-arg 3)) true false))

(defn dif-step [ out func state arg-num frame-shift ]
      (let [
      	   args (take arg-num state)
      	   out (if (= (count args) arg-num) (apply func args) nil)
	   ]
	   (list
		out
		func
		(reduce conj (multi-pop state frame-shift) (filter some? (if (coll? out) out (list out))))
		arg-num
		frame-shift)))

(defn diff-series ([ func init ]
      (diff-series func init (count init) 1))
      ([ func init prod-r ] (diff-series func init (count init) prod-r))
      ([ func init arg-num prod-r ]
      	 (map first (take-while enough-state?
	 	  (iterate (app-func dif-step) (list init func (queue init) arg-num prod-r))))))
;;examples

(defn stern-b [ aa bb ] (list (+ aa bb) bb))
;; stern-brocot (diff-series stern-b '(1 1))
;; fibonacci (diff-series + '(1 1))
;; unbound fibonacci (diff-series +' '(1 1))
;; lucas numbers (diff-series + '(2 1))
;; brady numbers https://www.youtube.com/watch?v=D8ntDpBm6Ok (diff-series + '(2308 4261))

(defn burrow-task [aa] (if (even? (count aa)) rest first))

(defn dec-seq [aa]
      (if (= 1 (first aa))
      	  (rest aa)
	  (flatten (list (- (first aa) 1) (rest aa)))))

(defn burrow [ structure & path ]
      (if (empty? path)
      	  (if (coll? structure) "coll" structure)
	  (recur ((burrow-task path) structure) (dec-seq path))))

(defn coll-wrap [aa] (if (coll? aa) (list aa) aa))
(defn burrow-rest [aa]
      (if (coll? aa) (if (empty? (rest aa)) "empty" (rest aa))))
      
(defn explore-seq [path struc]
      (if (coll? struc)
      	  (let [cc (first (first struc)) bb (burrow-rest (first struc)) ]
	       (list (format "(first %s)" path) (coll-wrap cc) (format "(rest %s)" path) (coll-wrap bb)))))

(defn base-conv [nn bb] (if (zero? nn) [] (conj (base-conv (quot nn bb) bb) (mod nn bb))))

(defn tree-122 [path goal depth] (let [max-depth (prob-122 goal)] (cond (= depth max-depth) nil (contains? path goal) path :else (filter some? (flatten (map (fn [path-next] (tree-122 path-next goal (inc depth))) (next-sets path)))))))

;;(defn next-sets [aa] (map #(conj aa %) (distinct (filter (fn [tt] (not (contains? aa tt))) (for [ii aa jj aa] (+ ii jj))))))
(defn next-sets [aa lim] (map #(conj aa %) (distinct (filter (fn [tt] (and (<= tt lim) (not (contains? aa tt)) (> tt (last aa)))) (for [ii aa jj aa] (+ ii jj))))))

(defn s-search-122 [[ss goal max-depth]] (let [depth (dec (count (first ss))) max-path (last (first ss)) curr-max-depth (peek max-depth)] (cond (empty? ss) [nil goal curr-max-depth] (or (> goal (bit-shift-left max-path (- curr-max-depth depth))) (>= depth curr-max-depth)) [(shift ss) goal max-depth] (contains? (first ss) goal) [(shift ss) goal (conj max-depth depth)] :else [(reduce conj (shift ss) (next-sets (first ss) goal)) goal max-depth])))

(defn shift [aa] (disj aa (first aa)))

(defn cond-dec [bool nn] (if bool (dec nn) nn))

(defn q-search-122 [[qq goal max-depth found-goal]]
  (cond
   (empty? qq) [nil goal (first max-depth) found-goal]
   (contains? (peek qq) goal) [(pop qq) goal (conj max-depth (dec (count (peek qq)))) true]
   (or (> goal (bit-shift-left (last (peek qq)) (- (first max-depth) (dec (count (peek qq)))))) (>= (dec (count (peek qq))) (cond-dec found-goal (first max-depth)))) [(pop qq) goal max-depth found-goal]
   :else [(reduce conj (pop qq) (next-sets (peek qq) goal)) goal max-depth found-goal]))

;;(defn q-search-122 [[qq goal max-depth]] (cond (empty? qq) [nil goal (peek max-depth)] (or (> goal (bit-shift-left (last (peek qq)) (- (peek max-depth) (dec (count (peek qq)))))) (>= (dec (count (peek qq))) (peek max-depth))) [(pop qq) goal max-depth] (contains? (peek qq) goal) [(pop qq) goal (conj max-depth (dec (count (peek qq))))] :else [(reduce conj (pop qq) (next-sets (peek qq) goal)) goal max-depth]))

(defn fix-point [func]
  (let [prev (atom nil)
        pred? (fn [xx]
                 (if (= @prev xx) false (swap! prev (fn [dd] xx))))]
                 (fn [dd] (last (take-while pred? (iterate func dd))))))

(defn sorting-sorted-sets [aa bb] (compare (vec aa) (vec bb)))

(defn bin-time-122 [aa] (+ (dec (bit-count aa)) (dec (count (base-conv aa 2)))))

(defn bit-count [aa] (reduce + (base-conv aa 2)))

(defn goal-122 [[path goal]]
  (cond
   (contains? path goal) path
   (> (first (max path)) goal) nil
   :else (map (fn [kk] [(conj path kk) goal])
	      (filter (fn [x?] (not (contains? path x?)))
		      (for [ii path jj path] (+ jj ii))))))

(defn prob-122 [aa bb]
  (let [cand
    (flatten
     (map
      (fn [ii]
	(let [goal (- bb ii)]
	     (map (fn [dd] (conj dd bb))
		  (filter (fn [jj] (contains? jj goal)) (get aa ii))))) (range 1 (inc (div-plus bb 2)))))
    out (first (min (map (fn [ii] (count (first ii))) cand)))]
    (reduce conj #{} (filter (fn [ii] (if (= out (count ii)) true false)) cand))))

(defn cond-assoc [pred-ish?? mm kk vv] (if (or (not (contains? mm kk)) (pred-ish?? vv (get mm kk))) (assoc mm kk vv) mm))
(defn smaller-set? [aa bb] (< (count aa) (count bb)))
(defn prob-122 [[aa bb]] [(reduce (fn debug [out ii] (cond-assoc smaller-set? out (+ bb ii) (conj (get aa bb) (+ bb ii)))) aa (get aa bb)) (inc bb)])

(def primes
     (lazy-seq
      (concat
       '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97))))

(defn prime? [aa]
  (not
   (first
    (filter
     #(if (= 0 (mod aa %)) true false)
     (take-while #(>= (Math/sqrt aa) %) primes)))))

(def primes
     (lazy-seq
      (concat
       '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
       (filter prime? (iterate (partial +' 2) 101)))))

(defn factor-prime [nn pp]
  (count (take-while #(= 0 (mod nn %)) (iterate (partial * pp) pp))))

;;(take 5 (map #(first %) (filter #(nth % 2) (iterate prime-seq-next [3 (sorted-set-by sorting-sorted-sets (sorted-set-by (neg-complement compare) 2 4)) false]))))

;;(defn y-comb [ff] (fn [aa] (last (take-while some? (iterate ff aa)))))

;; can't optimze with sqrt need recursive version to reduce problem size
;;(defn factor-map [aa] (reverse (rest (reverse (reduce (fn [out pp] (if (zero? pp) (assoc out (dec (count out)) (inc (last out))) (reduce conj out (list pp (inc (last out)))))) [1] (map #(factor-prime aa %) (take-while #(>= (Math/sqrt aa) %) primes)))))))

(defn prime [ii] (last (take ii primes)))

(defn exp [bb ee] (reduce *' (repeat ee bb)))

(defn auto-viv-inc [aa] (if (number? aa) (inc aa) 1))

(defn factor
  ([nn]
   (factor nn primes (sorted-map)))
  ([nn pp out]
       (let
           [work-prime (first pp)
           test (= 0 (mod nn work-prime))
           loc-nn (if test (quot nn work-prime) nn)
           loc-pp (if test pp (rest pp))
           loc-out (if test (assoc out work-prime (auto-viv-inc (get out work-prime))) out)]
           (if (= 1 nn) out (recur loc-nn loc-pp loc-out)))))

(defn prac?
  ([nn]
   (prac? (factor nn) 1))
  ([fact-map running-fact]
             (cond
              (empty? fact-map) true
              (> (first (first fact-map)) (inc running-fact)) false
              :else (recur (rest fact-map) (*' running-fact (exp (first (first fact-map)) (first (rest (first fact-map)))))))))

(defn next-sieve [nn ss]
  (if (not (= nn (first (first ss))))
    ss
    (recur nn (disj (conj ss (sorted-set-by (neg-complement compare) (+ (ffirst ss) (first (rest (first ss)))) (first (rest (first ss))))) (first ss)))))

(defn add-sieve [nn ss]
  (conj ss (sorted-set-by (neg-complement compare) nn ('* nn nn))))

(defn prime-seq-next [[nn ss bb]]
  (cond
   bb [(inc' nn) (add-sieve nn ss) false]
   (= (ffirst ss) nn) [(inc' nn) (next-sieve nn ss) false]
   :else [nn ss true]))

;;(take 5 (map #(first %) (filter #(nth % 2) (iterate prime-seq-next [3 (sorted-set-by sorting-sorted-sets (sorted-set-by (neg-complement compare) 2 4)) false]))))

;;(let [aa 600851475143] (reverse (rest (reverse (reduce (fn [out pp] (if (zero? pp) (assoc out (dec (count out)) (inc (last out))) (reduce conj out (list pp (inc (last out)))))) [1] (map #(factor-prime aa %) (take-while #(>= (Math/sqrt aa) %) primes)))))))

;;(last (take 10001 primes))

(defn prac?
  ([aa]
   (if (odd? aa)
       false
     (prac? (quot aa 2) 1 1 primes)))
  ([aa run-prod power prime-list]
       (cond
        (and (> (first prime-list) (inc run-prod)) (> run-prod 1)) false
        (= aa 1) true
        (= 0 (mod aa (first prime-list))) (prac? (quot aa (first prime-list)) run-prod (inc power) prime-list)
        :else (recur aa (*' run-prod (quot (-' (exp (first prime-list) (inc power)) 1) (-' (first prime-list) 1))) 0 (rest prime-list)))))

(defn eng-dream? [aa]
  (and
   (prac? (+' aa 8))
   (prac? (-' aa 4))
   (prac? (-' aa 8))
   (prac? (+' aa 4))
   (prac? aa)))

(def eng-dreams (filter eng-dream? (iterate (partial +' 4) 12)))

(defn trip-pair? [aa]
  (and
   (prime? (- aa 3))
   (prime? (+ aa 3))
   (prime? (- aa 9))
   (prime? (+ aa 9))))

(defn rough-log [aa]
  (count (take-while #(> aa %) (iterate (partial *' 10) 10))))

(defn newton [[nn guess]]
  [nn (-' guess (quot (-' guess (quot nn guess)) 2))])

(defn slice [aa bb]
  (if (map? aa) (map #(get aa %) bb) (map #(nth aa %) bb)))

(defn sum-num [aa] (reduce + 0 (filter number? aa)))

(defn fill-in [aa position goal top bottom]
  (let
      [r-c-d [#{0 1 2 3} #{4 5 6 7} #{8 9 10 11} #{12 13 14 15} #{0 4 8 12} #{1 5 9 13} #{2 6 10 14} #{3 7 11 15} #{0 5 10 15} #{3 6 9 12}]
      restrict-mag (map #(sum-num (slice aa %)) (filter #(contains? % position) r-c-d))
      free-slot (map #(count (filter nil? (slice aa %))) (filter #(contains? % position) r-c-d))
      high (min top (first (sort (map #(- goal %) restrict-mag))))
      low (max bottom (last (sort (map #(- goal %1 (* top (dec %2))) restrict-mag free-slot))))]
      (if (> low high) nil (map #(assoc aa position %) (range low (inc high))))))

(defn make-magic [candidates goal top bottom fill-plan]
  (if (empty? fill-plan)
    candidates
    (recur (filter some? (mapcat #(fill-in % (first fill-plan) goal top bottom) candidates)) goal top bottom (rest fill-plan))))
