(ns euler.core)

(use 'clojure.java.io)
(use '[clojure.string :only (split)])

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

;; #1:
(defn is-multiple-five-three [a]
  (or (= (rem a 5) 0) (= (rem a 3) 0)))

(reduce + (filter is-multiple-five-three (range 1000)))

;; #2
(defn fib [a b] (cons a (lazy-seq (fib b (+ b a)))))
(reduce + (filter even? (take-while (partial > 4000000) (fib 1 1))))

;; #3
(defn prime? [n]
  (if (= 2 n) true
    (if (even? n) false
        (let [root (num (int (Math/sqrt n)))]
          (loop [i 3]
            (if (> i root) true
              (if (zero? (mod n i)) false
                (recur (+ i 2)))))))))
(defn factor? [a b]
  (if (zero? a) false
    (zero? (mod b a))))
(defn prime-factor? [a b] (and (prime? a) (factor? a b)))
(defn factor-prime? [a b] (and (factor? b a) (prime? b)))

(defn sqrt-odd [a]
  (let [root (num (inc (int (Math/sqrt a))))]
    (if (even? root) (+ root 1) root)))

(defn largest-prime [a]
  (if (prime? a)
    a
    (let [root (sqrt-odd a)]
      (first (filter (partial factor-prime? a) (range root 1 -1))))))

;; questions: how to swap prime-factor? to factor-prime? easily

;; #4
(defn palindrome? [a]
  (let [string (str a)]
    (= (seq string) (reduce conj () string))))

(defn cross []
  (for [x (range 999 900 -1) y (range 999 900 -1)] [x y]))

(defn lg-palindrome []
  (take 1 (filter (fn [x] (palindrome? (* (first x) (last x)))) (cross))))

;; #5
;; (partial factor? x) -> "Multiple of x?"
(defn multiple? [a b] (factor? b a))
(defn multiple-of-every [b a] (every? (partial multiple? a) (range 2 b)))

(defn prime-factors [a]
  (filter (partial factor-prime? a) (range 1 a)))

(defn prime-factorization
  "Build the prime factorization"
  ([n]
    (let [afactor (largest-prime n)]
      (println (/ n afactor))
      (prime-factorization (/ n afactor) [afactor])))
  ([n acc]
    (if (= n 1)
      acc
      (let [afactor (largest-prime n)]
        (prime-factorization (/ n afactor) (conj acc afactor))))))

;; #7
;; 10001st prime
(defn euler-7 [nth]
  (loop [i 3 n 1]
    (if (= n nth)
      (println (dec i))
      (recur (inc i) (if (prime? i) (inc n) n)))))

;; #10
;; sum of primes below 2,000,000
(defn euler-10 []
  (+ 2
    (reduce +
      (filter prime?
        (range 3 2000000 2)))))


;; #11
(defn grid []
  (with-open [rdr (reader "./data/11.txt")]
    (doall
      (map (fn [a] (map #(Integer/parseInt %) (split a #" "))) (line-seq rdr)))))
(defn at-coord [x y]
  (nth (nth (grid) x) y))

(def vertical-products
  (for [x (range 0 16) y (range 0 20)]
    (* (at-coord x y) (at-coord (+ 1 x) y) (at-coord (+ 2 x) y) (at-coord (+ 3 x) y))))
(def horizontal-products
  (for [x (range 0 20) y (range 0 16)]
     (* (at-coord x y) (at-coord x (+ 1 y)) (at-coord x (+ 2 y)) (at-coord x (+ 3 y)))))
(def diagonal-down-products
  (for [x (range 0 16) y (range 0 16)]
    (* (at-coord x y) (at-coord (+ 1 x) (+ 1 y)) (at-coord (+ 2 x) (+ 2 y)) (at-coord (+ 3 x) (+ 3 y)))))
(def diagonal-up-products
  (for [x (range 4 20) y (range 0 16)]
    (* (at-coord x y) (at-coord (- x 1) (+ 1 y)) (at-coord (- x 2) (+ 2 y)) (at-coord (- x 3) (+ 3 y)))))

(def diagonal-products
  (concat diagonal-down-products diagonal-up-products))
(def products
  (concat vertical-products horizontal-products diagonal-products))

;; #12
;; (defn fib [a b] (cons a (lazy-seq (fib b (+ b a)))))
;; this factors is broken!! needs to go up to sqrt-odd, BUT add both factors for each
(defn factors [n]
  (concat
    (distinct
      (mapcat
        (fn [a] [a (/ n a)])
        (filter
          (fn [possible-factor] (factor? possible-factor n))
          (range 1 n))))))

(defn triangle-numbers
  ([] (triangle-numbers 1 1))
  ([a t]
    (cons t (lazy-seq (triangle-numbers (inc a) (+ t (inc a)))))))

(defn euler-12 [n]
  (take 1
    (filter
      (fn [number] (< n (count (factors number))))
      (triangle-numbers))))

;; 16

;; 21
(defn proper-divisors [n]
  (filter
    (fn [possible-factor] (factor? possible-factor n))
    (range 1 n)))

(defn divisors [n]
  (conj (proper-divisors n) n))

(defn d-of-n [n]
  (reduce + (proper-divisors n)))

(defn amicable? [a b]
  (and (not= a b)
       (and (= (d-of-n a) b)
            (= (d-of-n b) a))))

(defn problem-21 []
  (reduce +
    (distinct
      (filter (fn [a] (amicable? (d-of-n a) a)) (range 1 10000)))))

