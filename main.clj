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

(pack '(a a a a b c c a a d e e e e))

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
         (if (not (list? pair-or-single)) (list pair-or-single) (uncompress pair-or-single)))]
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
  (letfn [(helper [acc lst1 k]
            (cond
              (empty? lst1) acc
              (= k n) (helper acc (rest lst1) 1)
              :else (helper (concat acc (list (first lst1))) (rest lst1) (+ k 1)))
           )]
    (helper '() lst 1)))
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
  (let [k (rem n (len lst)) splat (split lst k)]
    (if (< n 0) (rotate lst (+ (len lst) n))
        (reduce concat (list (second splat) (first splat))))))

(rotate '(a b c d e f g h) -1)

;; Problem 20

(defn remove-at [lst n]
  (concat (slice lst 1 (dec n)) (slice lst (inc n) (len lst))))

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
  (let [pos (inc (rand-int (len lst))) ]
    (if (= n 0) '() (cons (element-at lst pos) (rnd-select (remove-at lst pos) (dec n))))))

;; Problem 24

(defn lotto-select [n m] (rnd-select (range m) n))

;; Problem 25

(defn rnd-permu [lst] (rnd-select lst (len lst)))

;; Problem 26

(defn combination [k lst]
  (cond
    (empty? lst) '()
    (= k 0) '()
    (= k 1) (map list lst)
    (> k (count lst)) '()
    :else (concat
           (map #(cons (first lst) %) (combination (dec k) (rest lst)))
           (combination k (rest lst)))))

;; Problem 27

;; Problem 28

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
