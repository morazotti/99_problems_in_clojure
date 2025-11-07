(require '[clojure.math :as m])

;; Problem 01

(defn my-last [lst]
  (if (empty? (rest lst)) (first lst) (my-last (rest lst))))

(my-last '(a b c d))

;; Problem 02

(defn my-but-last [lst]
  (cond
    (empty? lst) nil
    (empty? (rest lst)) nil
    (empty? (rest (rest lst))) lst
    :else (my-but-last (rest lst))))
(my-but-last '(a b c d))

;; Problem 03

(defn element-at [lst k]
  (letfn [(count-elements-get-kth [lst1 n]
            (cond
              (or (empty? lst1) (< n 1)) nil
              (= n 1) (first lst1)
              :else (count-elements-get-kth (rest lst1) (- n 1))))]
    (count-elements-get-kth lst k)))
(element-at '(a b c d) 3)

;; Problem 04

(defn len
  ([lst] (len lst 0))
  ([lst k] (cond
              (empty? lst) k
              :else (len (rest lst) (+ k 1)))))

;; Problem 05

(defn rev 
  ([lst] (rev lst '()))
  ([lst accumulator]
   (if (empty? lst) accumulator
       (rev
        (rest lst)
        (cons (first lst) accumulator)))))

;; Problem 06

(defn palindrome? [lst] (= lst (rev lst)))

;; Problem 07

(defn flatten [lst]
  (cond
    (not (list? lst)) (list lst)
    (empty? lst) '() 
    :else (concat (flatten (first lst)) (flatten (rest lst)))))

;; Problem 08

(defn compress [lst]
  (cond
    (empty? lst) lst
    (not (list? lst)) (list lst)
    (= (first lst) (second lst)) (compress (rest lst)) 
    :else (concat (list (first lst)) (compress (rest lst)))))

;; Problem 09

(defn pack
  ([lst] (pack lst '()))
  ([lst accumulator]
   (cond
     (empty? lst) accumulator
     (= (first lst) (second lst)) (pack (rest lst) (concat (list (first lst)) accumulator))
     :else (cons (concat (list (first lst)) accumulator) (pack (rest lst))))))

(compress '(a a a a b c c a a d e e e e))

;; Problem 10

(defn encode
  ([lst] (encode lst 1))
  ([lst n]
   (cond
     (empty? lst) nil
     (= (first lst) (second lst)) (encode (rest lst) (inc n))
     :else (cons (list n (first lst)) (encode (rest lst))))))
(encode '(a a a a b c c a a d e e e e))

;; Problem 11

(defn encode-modified [lst]
  (letfn [(modify [pair]
            (if (= 1 (first pair)) (second pair) pair))]
    (map modify (encode lst))))
(encode-modified '(a a a a b c c a a d e e e e))

;; Problem 12

(defn decode [encoded]
  (letfn
      [(dec-pair [pair]
         (list (dec (first pair))
               (second pair)))
       (uncompress [pair]
         (if (= (first pair) 1)
           (list (second pair))
           (concat (list (second pair))
                   (uncompress (dec-pair pair)))))
       (decode-one [pair-or-single]
         (if (not (list? pair-or-single))
           (list pair-or-single)
           (uncompress pair-or-single)))]
    (reduce concat (map decode-one encoded))))

;; Problem 14

(defn duplicate [lst]
  (let [[x & ys] lst]
    (if (empty? lst) lst (cons x (cons x (duplicate ys))))))

;; Problem 15

(defn replicate [lst n]
 (letfn [(iter [k]
           (cond
             (empty? lst) nil 
             (= 1 k) (cons (first lst) (replicate (rest lst) n))
             :else (cons (first lst) (iter (dec k)))))]
   (iter n)))
(replicate '(a b c) 3)



;; Problem 16
(defn drop-nth [lst n]
  (letfn [(iter [acc lst1 k]
            (cond
              (empty? lst1) acc
              (= k n) (iter acc (rest lst1) 1)
              :else (iter (concat acc (list (first lst1)))
                          (rest lst1) (+ k 1))))]
    (iter '() lst 1)))
(drop-nth '(a b c d e f g h i k) 3)
  
;; Problem 17
(defn split [lst n]
  (letfn [(first-part [k lst1 acc]
            (if (= k 0) acc
                (first-part (- k 1) (rest lst1) (concat acc (list (first lst1))))))
          (second-part [k lst2]
            (if (> k 0) (second-part (- k 1) (rest lst2)) lst2))]
    (list (first-part n lst '()) (second-part n lst))))

(split '(a b c d e f g h i k) 3)


;; Problem 18
(defn slice [lst start end]
  (letfn [(iter [n acc lst1]
            (cond
              (> n end) acc 
              (< n start) (iter (inc n) acc (rest lst1))
              :else (iter (inc n) (concat acc (list (first lst1)))
                          (rest lst1))))]
    (iter 1 '() lst)))

;; Problem 19

(defn rotate [lst n]
  (let [k (rem n (count lst)) splat (split lst k)]
    (if (< n 0) (rotate lst (+ (count lst) n))
        (reduce concat (list (second splat) (first splat))))))

(rotate '(a b c d e f g h) -1)

;; Problem 20

(defn remove-at [lst n]
  (concat (slice lst 1 (dec n)) (slice lst (inc n) (count lst))))

(remove-at '(a b c d) 1)

;; Problem 21

(defn insert-at [value lst n]
  (let [[fst snd] (split lst (dec n))]
    (concat fst (list value) snd)))
(insert-at 'alpha '(a b c d) 1)


;; Problem 22

(defn my-old-range [lo hi]
  (if (> lo hi) '()
      (cons lo (my-old-range (inc lo) hi))))

(defn my-range
  ([hi] (my-range 1 hi))
  ([lo hi] (letfn [(natural
            ([] (natural 1))
            ([n] (lazy-seq (cons n (natural (inc n))))))]
    (take (inc (- hi lo)) (natural lo)))))

;; Problem 23

(defn rnd-select [lst n]
  (let [pos (inc (rand-int (count lst))) ]
    (if (= n 0) '() (cons (element-at lst pos) (rnd-select (remove-at lst pos) (dec n))))))

;; Problem 24

(defn lotto-select [n m] (rnd-select (range m) n))

;; Problem 25

(defn rnd-permu [lst] (rnd-select lst (len lst)))

;; Problem 26

(defn combination [k lst]
  (cond
    (empty? lst) '()
    (zero? k) '()
    (= 1 k) (map list lst)
    (> k (count lst)) '()
    :else (concat
           (map #(cons (first lst) %) (combination (dec k) (rest lst)))
           (combination k (rest lst)))))

;; Problem 27
(defn group [lst amnt]
  (if (empty? amnt) '(())
      (letfn [(get-elems-from-position-list [positions]
                (map #(element-at lst %) positions))

              (remove-elems-from-position-list [positions]
                (loop [lst lst
                       positions (reverse positions)]
                 (if (or (empty? positions) (empty? lst)) lst (recur (remove-at lst (first positions)) (rest positions)))))]

        (let [elems (my-range 1 (count lst))
              combs (combination (first amnt) elems)
              lst-chosen (map get-elems-from-position-list combs)
              lst-remaining (map remove-elems-from-position-list combs)
              lst-elems (map #(cons %1 (list %2)) lst-chosen lst-remaining)
              ]
          (mapcat
           (fn [[chosen remaining]]
             (if (empty? (rest amnt)) (list (list chosen))
                 (map #(cons chosen %) (group remaining (rest amnt)))))
           lst-elems)))))

(defn group-cgpt [lst amnt]
  (if (empty? amnt) '(())
      (mapcat
       (fn [comb]
         (let [remaining
               (reduce (fn [acc elems] (remove #{elems} acc)) lst comb)]
           (map #(cons comb %) (group-cgpt remaining (rest amnt)))))
       (combination (first amnt) lst))))


(group-cgpt '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))

;; Problem 28 a

(defn lsort
  ([coll] (lsort coll identity))
  ([coll f]
   (letfn [(smallest? [x coll1]
             (cond
               (empty? coll1) true
               (> (f x) (f (first coll1))) false
               :else (smallest? x (rest coll1))))]

     (loop [coll coll acc '()]
       (cond
         (empty? coll) acc 
         (smallest? (first coll)
                    (rest coll))
         (recur (rest coll)
                (concat acc (list (first coll))))
         :else (recur (rotate coll 1) acc))))))

(lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)) count)

;; Problem 28 b

(defn lfsort [l]
  (let [freq-of-lens (lsort (encode (lsort (map count l))) first)]
    (letfn [(get-elems-with-length [length lst]
              (cond (empty? lst) '()
                    (= length (count (first lst)))
                    (cons (first lst) (get-elems-with-length length (rest lst)))
                    :else (get-elems-with-length length (rest lst))))]
      (mapcat #(get-elems-with-length (second %) l) freq-of-lens))))

(lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))

;; Problem 31

(defn next-factor [n] (if (= n 2) 3 (+ 2 n)))
(defn divides? [n a] (= 0 (mod n a)))
(defn square [n] (* n n))
(defn find-divisor [n test-divisor]
              (cond
                (> (square test-divisor) n) n 
                (divides? n test-divisor) test-divisor 
                :else (recur n (next-factor test-divisor))))
(defn smallest-divisor [n] (find-divisor n 2))

(defn prime? [n]
    (if (= n 1) false (= n (smallest-divisor n))))

;; Problem 32

(defn gcd [a b]
  (if (= 0 b) a (gcd b (rem a b))))

;; Problem 33
(defn coprime [a b]
  (= 1 (gcd a b)))

;; Problem 34
(defn phi [n]
  (len (filter #(not (nil? %))
               (map #(when (coprime n %) %)
                    (my-range 1 (dec n))))))

;; Problem 35

(defn prime-factors [n]
  (let [x (smallest-divisor n)]
    (if (= 1 n) '()
        (concat (list x) (prime-factors (/ n x))))))

;; Problem 36

(defn prime-factors-mult [n]
  (map #(list (second %) (first %))
       (encode (prime-factors n))))

(prime-factors-mult 315)

;; Problem 37

(defn phimproved [m]
  (let [factors (prime-factors-mult m)]
    (letfn [(mult-and-pow [[p m]]
             (* (dec p) (m/pow  p (dec m))))]
      (reduce * (map mult-and-pow factors)))))

;; Problem 38
(phimproved 10090) ;; => 4032
(phi 10090) ;; => StackOverflow

;; Problem 39
(defn primes
  ([hi] (primes 1 hi))
  ([lo hi] (filter prime? (my-range lo hi))))

;; Problem 40

(defn goldbach
  ([n] (goldbach n (primes n)))
  ([n primes]
   (let [x (first primes) diff (- n x)]
     (if (prime? diff) (list x diff) (goldbach n (rest primes))))))

;; Problem 41

(defn big-primes?
  ([lst] (big-primes? lst 50))
  ([lst v] (let [fsecond (comp first second)]
             (> (fsecond lst) v))))

(defn goldbach-list
  ([hi] (goldbach-list 1 hi))
  ([lo hi]
   (map #(list % (goldbach %)) (filter even? (my-range lo hi))))
  ([lo hi minimum] (filter #(big-primes? % minimum)
                           (goldbach-list 1 hi))))

(len (filter big-primes? (goldbach-list 2 3000 50))) ;; => 10
(len (filter big-primes? (goldbach-list 2 9000 50))) ;; => 81

;; Problem 46

(defn nor [x y] (not (or x y)))
(defn xor [x y] (or (and x (not y))
                    (and (not x) y)))
(defn impl [x y] (or x (not y)))
(defn equ [x y] (not (xor x y)))

(defn substitute [expr old new]
  (cond
    (= expr old) new
    (list? expr) (map #(substitute % old new) expr)
    :else expr))

(defn table [A B expr]
  (let [a '(true false)
        b '(true false)]
    (map (fn [e] (map #(list % (substitute e B %))) b) (map #(substitute expr A %) a))))

(table 'A 'B '(and A B))

;; Problem 54A

(defn tree? [tree]
  (cond
    (nil? tree) true
    (= 3 (count tree)) (apply (fn [x y] (and x y)) (map tree? (rest tree)))
    :else false))

;; for fun
(defn invert-tree [tree]
  (let [[a b c] tree]
    (cond
      (nil? tree) nil 
      :else (list a (invert-tree c) (invert-tree b)))))

;; Problem 55

;; Problem 56

;; Problem 61

(defn first-count-leaves
  ([tree] (count-leaves tree 0))
  ([tree n]
   (let [[a b c] tree]
     (cond
       (empty? tree) n
       (and (nil? b) (nil? c)) (inc n) 
       :else (apply + (map #(count-leaves % n) (rest tree))))
     )))

;; Problem 61A
(defn leaves
  ([tree] (leaves tree '()))
  ([tree acc]
   (let [[a b c] tree]
     (cond
       (nil? tree) acc
       (and (nil? b) (nil? c)) (cons tree acc)
       :else (mapcat #(leaves % acc) (rest tree))))))

;; knowing that `leaves` collects the leaves, it suffices to count them
(defn count-leaves [tree] (count (leaves tree)))

;; Problem 62

;; Problem 70B
(defn multi-tree? [tree]
  (cond
    (empty? tree) true
    (not (list? (first tree))) true
    (and (list? tree) (>= (count tree) 2) (not (list? (first tree))) (multi-tree? (rest tree))) true
    :else false)
  )
(multi-tree? '((a b) b c (d e)))

;; Problem 90

;; Problem 93

(defn arith-puzzle [list-num] 
  (let [ops '(+ - * /)]
    (letfn [(possible-splits [l]
             (map #(split l %)
                  (my-range (dec (count l)))))

           (apply-ops [l]
             (cond
               (empty? l) '()
               (not (list? l)) l
               (= 1 (count l)) (first l) 
               :else
               (cons
                `(= (~(first ops) ~l) ~l
                      ) '()
                )

               )
             )]
      (map apply-ops (possible-splits list-num)))))
(arith-puzzle '(1 2 3 4))

;; Problem 95

(defn n-to-list
  ([number] (n-to-list number '()))
  ([number acc]
   (if (< number 1) acc
      (let [unit (rem number 10)]
        (n-to-list (/ (- number unit) 10)
                   (cons unit acc))))))

(defn full-words [number]
  (let [n (n-to-list number)]
   (letfn [(number-to-word [nmb]
             (cond
               (= nmb 0) "zero"
               (= nmb 1) "one"
               (= nmb 2) "two"
               (= nmb 3) "three"
               (= nmb 4) "four"
               (= nmb 5) "five"
               (= nmb 6) "six"
               (= nmb 7) "seven"
               (= nmb 8) "eight"
               (= nmb 9) "nine"))
           (join-string-list [l]
             (if (= 1 (count l)) (first l)
                 (apply str (first l) "-"
                        (join-string-list (rest l)))))]
     (join-string-list (map number-to-word n)))))

;; idiomatic way to rewrite
;; didn't know that ([vec] n) gave the nth position of [vec]
;; also, Character/digit was new to me

(defn full-words2 [number]
  (let [words ["zero" "one" "two" "three" "four"
               "five" "six" "seven" "eight" "nine"]]
    (->> (str number)
         (map #(Character/digit % 10))
         (map words)
         (clojure.string/join "-"))))


;; Problem 97
(ns sudoku.core)

(defn empty-pos [g]
  ;; primeira posição com 0, ou nil se não há
  (some (fn [[r c]] (when (zero? (get-in g [r c])) [r c]))
        (for [r (range 9), c (range 9)] [r c])))

(defn row-ok? [g r n] (not-any? #{n} (g r)))
(defn col-ok? [g c n] (not-any? #{n} (map #(get-in g [% c]) (range 9))))
(defn block-ok? [g r c n]
  (let [x0 (* 3 (quot c 3))
        y0 (* 3 (quot r 3))]
    (not-any? #{n} (for [i (range 3), j (range 3)]
                     (get-in g[(+ y0 i) (+ x0 j)])))))

(defn valid? [g r c n]
  (and (row-ok? g r n)
       (col-ok? g c n)
       (block-ok? g r c n)))g

(defn solve [g]
  (if-let [[r c] (empty-pos g)]
    (mapcat
     identity
     (for [n (range 1 10) :when (valid? g r c n)]
       (solve (assoc-in g [r c] n))))
    g))


(clojure.pprint/pprint
 (let [grid
       [[0 7 2 0 0 5 0 0 0]
        [9 0 0 0 0 0 0 8 0]
        [0 0 0 0 7 0 0 0 0]
        [0 0 0 2 0 0 3 0 5]
        [3 0 0 0 1 0 7 0 0]
        [0 0 0 0 0 4 8 0 0]
        [1 0 0 0 0 6 0 0 9]
        [0 0 6 1 0 0 0 0 0]
        [0 0 0 3 4 0 0 0 2]]]
  (solve grid)))
