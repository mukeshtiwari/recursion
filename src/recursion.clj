(ns recursion)

(defn product [coll]
  (apply * coll))

(product [])        ;=> 1  ; special case
(product [1 2 3])   ;=> 6
(product [1 2 3 4]) ;=> 24
(product [0 1 2])   ;=> 0
(product #{2 3 4})  ;=> 24 ; works for sets too!

(defn singleton? [coll]
  (cond
    (empty? coll) false
    (empty? (rest coll)) true
    :else false))




(singleton? [1])     ;=> true
(singleton? #{2})    ;=> true
(singleton? [])      ;=> false
(singleton? [1 2 3]) ;=> false

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(my-last [])      ;=> nil
(my-last [1 2 3]) ;=> 3
(my-last [2 5])   ;=> 5

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))

(max-element [2 4 1 4]) ;=> 4
(max-element [2])       ;=> 2
(max-element [])        ;=> nil


(defn seq-max [seq-1 seq-2]
  (let [lcount (count seq-1)
        rcount (count seq-2)]
    (if (> lcount rcount)
      seq-1
      seq-2)))


(seq-max [1] [1 2])   ;=> [1 2]
(seq-max [1 2] [3 4]) ;=> [3 4]

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(longest-sequence [[1 2] [] [1 2 3]]) ;=> [1 2 3]
(longest-sequence [[1 2]])            ;=> [1 2]
(longest-sequence [])


(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    :else
      (let [afirst (first a-seq)
            arest (rest a-seq)]
        (if (pred? afirst)
            (cons afirst (my-filter pred? arest))
            (my-filter pred? arest)))))

(my-filter odd? [1 2 3 4]) ;=> (1 3)
(my-filter (fn [x] (> x 9000)) [12 49 90 9001]) ;=> (9001)
(my-filter even? [1 3 5 7]) ;=> ()


(defn sequence-contains? [elem a-seq]
  (not (empty? (my-filter (fn [x] (= x elem)) a-seq))))

(sequence-contains? 3 [1 2 3]) ;=> true
(sequence-contains? 3 [4 7 9]) ;=> false
(sequence-contains? :pony [])  ;=> false

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    :else
      (if (pred? (first a-seq))
          (cons (first a-seq) (my-take-while pred? (rest a-seq)))
          ())))

(my-take-while odd? [1 2 3 4])  ;=> (1)
(my-take-while odd? [1 3 4 5])  ;=> (1 3)
(my-take-while even? [1 3 4 5]) ;=> ()
(my-take-while odd? [])         ;=> ()

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    :else
      (let [afirst (first a-seq)
            arest (rest a-seq)]
        (if (pred? afirst)
            (my-drop-while pred? arest)
            a-seq))))


(my-drop-while odd? [1 2 3 4])  ;=> (2 3 4)
(my-drop-while odd? [1 3 4 5])  ;=> (4 5)
(my-drop-while even? [1 3 4 5]) ;=> (1 3 4 5)
(my-drop-while odd? [])         ;=> ()


(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    :else (and (= (first a-seq) (first b-seq))
               (seq= (rest a-seq) (rest b-seq)))))

(seq= [1 2 4] '(1 2 4))  ;=> true
(seq= [1 2 3] [1 2 3 4]) ;=> false
(seq= [1 3 5] [])        ;=> false

(defn my-map [f seq-1 seq-2]
    (cond
      (or (empty? seq-1) (empty? seq-2)) ()
      :else (cons (f (first seq-1) (first seq-2))
                  (my-map f (rest seq-1) (rest seq-2)))))

(my-map + [1 2 3] [4 4 4])   ;=> (5 6 7)
(my-map + [1 2 3 4] [0 0 0]) ;=> (1 2 3)
(my-map + [1 2 3] [])        ;=> ()

(defn power [n k]
  (if (= k 0)
      1
      (* n (power n (- k 1)))))

(power 2 2)  ;=> 4
(power 5 3)  ;=> 125
(power 7 0)  ;=> 1
(power 0 10) ;=> 0

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else
      (+ (fib (- n 1)) (fib (- n 2)))))


(fib 0) ;=> 0
(fib 1) ;=> 1
(fib 2) ;=> 1
(fib 3) ;=> 2
(fib 4) ;=> 3
(fib 5) ;=> 5
(fib 6) ;=> 8
(fib 10) ;=> 55

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (<= how-many-times 0) ()
    :else (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(my-repeat 2 :a)    ;=> (:a :a)
(my-repeat 3 "lol") ;=> ("lol" "lol" "lol")
(my-repeat -1 :a)   ;=> ()

(defn my-range [up-to]
  (if (<= up-to 0)
      ()
      (cons (- up-to 1) (my-range (- up-to 1)))))

(my-range 0)  ;=> ()
(my-range 1)  ;=> (0)
(my-range 2)  ;=> (1 0)
(my-range 3)  ;=> (2 1 0)

(defn tails [a-seq]
  (if (empty? a-seq)
      '(())
      (cons (seq a-seq) (tails (rest a-seq)))))

(tails [1 2 3 4]) ;=> ((1 2 3 4) (2 3 4) (3 4) (4) ())

(defn inits [a-seq]
  (butlast (reverse (cons () (map reverse (tails (reverse a-seq)))))))

(inits [1 2 3 4]) ;=> (() (1) (1 2) (1 2 3) (1 2 3 4))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (butlast (map concat (tails a-seq) (inits a-seq)))))


(inits [1 2 3])
(tails [1 2 3])

(rest '(()))
(rotations [])        ;=> (())
(rotations [1 2 3])   ;=> ((1 2 3) (2 3 1) (3 1 2))
(rotations [:a :b])   ;=> ((:a :b) (:b :a))
; The order of rotations does not matter.
(rotations [:a :b])   ;=> ((:b :a) (:a :b))
(rotations [1 5 9 2]) ;=> ((1 5 9 2) (2 1 5 9) (9 2 1 5) (5 9 2 1))
(count (rotations [6 5 8 9 2])) ;=> 5

(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq) freqs
    :else
      (let [akey (first a-seq)
            nfreqs
              (if (contains? freqs akey)
                  (assoc freqs akey (inc (get freqs akey)))
                  (assoc freqs akey 1))]
        (my-frequencies-helper nfreqs (rest a-seq)))))



(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(my-frequencies []) ;=> {}
(my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}


(defn un-frequencies [a-map]
  (if
    (empty? a-map) '()
    (let [[first-elem first-val] (first a-map)]
      (concat (repeat first-val first-elem) (un-frequencies (rest a-map))))))

(un-frequencies {:a 3 :b 2 "^_^" 1})             ;=> (:a :a :a "^_^" :b :b)
(un-frequencies (my-frequencies [:a :b :c :a]))  ;=> (:a :a :b :c)
(my-frequencies (un-frequencies {:a 100 :b 10})) ;=> {:a 100 :b 10}

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
      '()
      (cons (first coll) (my-take (dec n) (rest coll)))))

(my-take 2 [1 2 3 4]) ;=> (1 2)
(my-take 4 [:a :b])   ;=> (:a :b)

(defn my-drop [n coll]
  (if (<= n 0)
      coll
      (my-drop (dec n) (rest coll))))

(my-drop 2 [1 2 3 4]) ;=> (3 4)
(my-drop 4 [:a :b])   ;=> ()

(defn halve [a-seq]
  (let [n (count a-seq)
        half (int (/ n 2))]
    (vector (my-take half a-seq) (my-drop half a-seq))))

(halve [1 2 3 4])   ;=> [(1 2) (3 4)]
(halve [1 2 3 4 5]) ;=> [(1 2) (3 4 5)]
(halve [1])         ;=> [() (1)]

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else
      (let [a (first a-seq)
            b (first b-seq)]
        (if (<= a b)
            (cons a (seq-merge (rest a-seq) b-seq))
            (cons b (seq-merge a-seq (rest b-seq)))))))

(seq-merge [4] [1 2 6 7])        ;=> (1 2 4 6 7)
(seq-merge [1 5 7 9] [2 2 8 10]) ;=> (1 2 2 5 7 8 9 10)

(defn merge-sort [a-seq]
   (cond
     (empty? a-seq) '()
     (singleton? a-seq) a-seq
     :else
        (let [[fhalf shalf] (halve a-seq)]
          (seq-merge (merge-sort fhalf) (merge-sort shalf)))))



(inits [0 5 4 7 1 3])

(merge-sort [])                 ;=> ()
(merge-sort [1 2 3])            ;=> (1 2 3)
(merge-sort [5 3 4 17 2 100 1]) ;=> (1 2 3 4 5 17 100)

(defn split-into-monotonics [a-seq]
  [:-])



(defn combine [el lst]
           (map (fn [x] (concat (list el) x)) lst))

;; Test cases are probably list or vector so change them to list
(defn permutations [ast]
  (let [a-set (into #{} ast)]
    (cond
      (empty? a-set) '(())
      (singleton? a-set) (list (into '() a-set))
      :else
        (mapcat (fn [x] (combine x (permutations (disj a-set x)))) a-set))))

(permutations #{})

(permutations #{1 5 2 4})

(defn powerset [s]
  (if (empty? s) (list '())
      (let [ps (powerset (rest s))]
        (concat ps
                (map (fn [p] (cons (first s) p)) ps)))))


(powerset #{})      ;=> #{#{}}
(powerset #{1 2 4}) ;=> #{#{} #{4} #{2} #{2 4} #{1} #{1 4} #{1 2} #{1 2 4}}



