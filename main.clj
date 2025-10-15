;; Problem 01

(defn my-last [lst]
  (if (empty? (rest lst)) (first lst) (my-last (rest lst))))

;; Problem 02

(defn my-but-last [lst]
  (cond
    (empty? lst) nil
    (empty? (rest lst)) nil
    (empty? (rest (rest lst))) lst
    :else (my-but-last (rest lst))))

;; Problem 03

(defn element-at [lst k]
  (letfn [(helper [lst1 n]
            (cond
              (or (empty? lst1) (< n 1)) nil
              (= n 1) (first lst1)
              :else (helper (rest lst1) (- n 1))))]
    (helper lst k)))

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

;; Problem 14

(defn duplicate [lst]
  (let [[x & ys] lst]
    (if (empty? lst) lst (cons x (cons x (duplicate ys))))))

;; Problem 15
(defn replicate [lst n]
  (letfn [(helper [acc k elem]
            (if (= k 0) acc
                (helper (cons elem acc) (- k 1) elem)))]
    (map #(helper '() n %) lst)))

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
  
;; Problem 22
(defn my-range [lo hi]
  (if (> lo hi) '()
      (cons lo (my-range (inc lo) hi))))
