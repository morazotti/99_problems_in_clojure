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
  (letfn [(helper [lst1 n]
            (cond
              (or (empty? lst1) (< n 1)) nil
              (= n 1) (first lst1)
              :else (helper (rest lst1) (- n 1))))]
    (helper lst k)))
(element-at '(a b c d) 3)

;; Problem 04

(defn len [lst]
  (letfn [(helper [lst1 k]
            (cond
              (empty? lst1) k
              :else (helper (rest lst1) (+ k 1))))]
  (helper lst 0)))

;; Problem 05

(defn rev [lst]
  (letfn
      [(rev-helper [lst1 acc]
         (if (empty? lst1) acc
             (rev-helper
              (rest lst1)
              (cons (first lst1) acc))))]
    (rev-helper lst '())))

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

(defn pack [lst]
  (letfn [(iter [lst2 acc]
            (cond
              (empty? lst2) acc
              (= (first lst2) (second lst2)) (iter (rest lst2) (concat (list (first lst2)) acc))
              :else (cons (concat (list (first lst2)) acc) (pack (rest lst2))))
            )]
    (iter lst '())))
(pack '(a a a a b c c a a d e e e e))

;; Problem 10

(defn encode [lst]
  (letfn [(iter [lst2 n]
            (cond
              (empty? lst2) nil
              (= (first lst2) (second lst2)) (iter (rest lst2) (inc n))
              :else (cons (list n (first lst2)) (encode (rest lst2))))
            )]
    (iter lst 1)))
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
  (if (< n 0) (rotate lst (+ (len lst) n))
      (reduce concat (split lst n))))
(rotate '(a b c d e f g h) -2)

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
(defn my-range [lo hi]
  (if (> lo hi) '()
      (cons lo (my-range (inc lo) hi))))

;; Problem 23

(defn rnd-select [lst n]
  (let [pos (inc (rand-int (len lst))) ]
    (if (= n 0) '() (cons (element-at lst pos) (rnd-select (remove-at lst pos) (dec n))))
    ))

;; Problem 24

(defn lotto-select [n m] (rnd-select (range m) n))

;; Problem 25

(defn rnd-permu [lst] (rnd-select lst (len lst)))

;; Problem 26

(defn combination [k lst]
  (cond
    (= k (len lst)) lst
    (= k 0) '()
    (empty? lst) '()
    :else (cons (combination k (rest lst)) (cons (first lst) (combination (dec k) (rest lst))))))
(combination 2 '(w u r b g))
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
