;(def bill [{:name "Green Tea Ice Cream" :price 2.5 :quantity 2}
;           {:price 1.0 :name "Sticky Rice" :quantity 1}])
;(def items [{:price 2.1 :name "Mango" :quantity 1} {:quantity 1 :price 1.0 :name "Sticky Rice"}])

; Question 1
(defn bill-total [bill]
  ;; map lambda to bill, then reduce with +
  ;; the lambda calculate (* price quantity) on item
  (reduce + (map #(* (:price %) (:quantity %)) bill)))


; Question 2
(defn add-to-bill [bill items]
  ;; get the names set of bill
  (let [names (set (map :name bill))]
    (reduce
     (fn [new-bill item]
       ;; for each item in new-bill
       (if (contains? names (:name item))
         ;; if the item is already in bill, update the quantity
         (mapv
          ;; use the update function, update quantity if same item, or use origal quantity
          (fn [x] (update x :quantity
                          #(if (= (:name x) (:name item))
                             (+ % (:quantity item)) %)))
          new-bill)
         ;; the item not in bill, conjunction it to new-bill
         (conj new-bill item)))
     ;; use bill as initial value of new-bill
     bill
     items)))


; Question 3
(defn divisors [n]
  ;; reverse to asure ascending order
  (reverse
   ;; start from 1
   (loop [x 1
          acc '()]
     (cond
       ;; x is greater than n, done
       (> x n) acc
       ;; x is a divisor of n
       (zero? (mod n x)) (recur (inc x) (conj acc x))
       ;; x is not a divisor of n
       :else (recur (inc x) acc)))))

; Question 4
(defn abundance [n]
  ;; sum divisors, substract double of n
  (- (reduce + (divisors n)) (* n 2)))

; Question 5
;; use filter to find the number which is has positive abundance
(filter #(> (abundance %) 0) (range 1 300))

; Question 6
(defn pattern-count [text pattern]
  ;; get the string length of text and pattern
  (let [n (count text)
        m (count pattern)]
    ;; start from pos 0 of text, and initial count is 0
    (loop [start 0
           count 0]
      (cond
        ;; can't search pattern anymore, done
        (> (+ start m) n) count
        ;; found pattern
        (= (subs text start (+ start m)) pattern) (recur (inc start) (inc count))
        ;; not found
        :else (recur (inc start) count)))))