;; Only solutions that require some form of code will be written here

;; 15. Write a function which doubles a number
(fn [x] (* 2 x))

;; 16. Write a function which returns a personalized greeting.
(fn [x] (str "Hello, " x "!"))

;; 19. Write a function which returns the last element in a sequence.
#(nth % (dec (count %)))

;; 20. Write a function which returns the second to last element from a sequence.
#(nth % (dec (dec (count %))))

;; 21. Write a function which returns the Nth element from a sequence.
(fn [s n] (if (= n 0) (first s) (recur (rest s) (dec n))))

;; 22. Write a function which returns the total number of elements in a sequence.
(fn [lst]
	(loop [len 0 x lst]
		(if (empty? x)
			len
			(recur (+ 1 len) (rest x)))))

;; 23. Write a function which reverses a sequence.
(fn [lst]
	(loop [l1 lst l2 []]
		(if (empty? l1)
			l2
			(recur (rest l1) (concat [(first l1)] l2)))))

;; 24. Write a function which returns the sum of a sequence of numbers.
reduce (fn [sum x] (+ sum x))

;; 27. Write a function which returns true if the given sequence is a palindrome.
(fn [l] (= (reverse l) (reverse (reverse l))))

;; 26. Write a function which returns the first X fibonacci numbers.
(fn [n] (take n (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1]))))

;; 38. Write a function which takes a variable number of parameters and returns the maximum value.
(fn [& args] (reduce (fn [x y] (if (> x y)  x y)) args))

;; 29. Write a function which takes a string and returns a new string containing only the capital letters.
(fn [x]
	(apply str (filter #(Character/isUpperCase %) x)))

;; 134. Write a function which, given a key and map, returns true iff the map contains an entry with that key and its value is nil.
(fn [k s]
	(and (contains? s k) (nil? (get s k))))

;; 32. Write a function which duplicates each element of a sequence.
(fn [x] (apply concat (map #(vector % %) x)))

;; 34. Write a function which creates a list of all integers in a given range.
(fn [start end]
	(loop [i start lst []]
		(if (= i end)
			lst
			(recur (+ i 1) (conj lst i)))))

;; 42. Write a function which calculates factorials.
(fn [n]
	(reduce #(* %1 %2) (range 1 (inc n))))

;; 28. Write a function which flattens a sequence.
(fn [x]
	(filter (complement sequential?)
		(rest (tree-seq sequential? seq x))))

;; 39. Write a function which takes two sequences and returns the first item from each, then the second item from each, then the third, etc.
(fn zip [c1 c2]
	(let [s1 (seq c1) s2 (seq c2)]
		(when (and s1 s2)
			(cons (first s1)
				(cons (first s2)
					(zip (rest s1) (rest s2)))))))

;; 30. Write a function which removes consecutive duplicates from a sequence.
(fn [x] (map first (partition-by identity x)))

;; 33. Write a function which replicates each element of a sequence a variable number of times.
(fn [x n] (mapcat #(repeat n %) x))

;; 40. Write a function which separates the items of a sequence by an arbitrary value.
(fn comb [s x] (butlast (interleave x (repeat (count x) s))))

;; 31. Write a function which packs consecutive duplicates into sub-lists.
partition-by identity

;; 41. Write a function which drops every Nth item from a sequence.
(fn [coll n]
	(flatten
		(concat
			(map #(drop-last %) (partition n coll))
			(take-last (rem (count coll) n) coll))))

;; 49. Write a function which will split a sequence into two parts.
(fn [n lst]
	(vector (take n lst) (drop n lst)))

;; 34. Write a function which creates a list of all integers in a given range.
(fn [start end]
	(loop [i start lst []]
		(if (= i end)
			lst
			(recur (+ i 1) (conj lst i)))))

;; 166. For any orderable data type it's possible to derive all of the basic comparison operations (<, ≤, =, ≠, ≥, and >) from a single operation (any operator but = or ≠ will work). Write a function that takes three arguments, a less than operator for the data and two items to compare. The function should return a keyword describing the relationship between the two items. The keywords for the relationship between x and y are as follows:
(fn [f v1 v2]
	(keyword (if (f v1 v2)
		"lt"
		(if (f v2 v1)
			"gt"
			"eq"))))

;; 83. Write a function which takes a variable number of booleans. Your function should return true if some of the parameters are true, but not all of the parameters are true. Otherwise your function should return false.
(fn [& args]
	(if (nil?
		(and
			(some true? args)
			(some false? args)))
	false
	true))

;; 61. Write a function which takes a vector of keys and a vector of values and constructs a map from them.
(fn zipmap-2 [k v]
	(loop [ks k vs v m {}]
		(if (or (empty? ks) (empty? vs))
			m
			(recur (rest ks) (rest vs) (into m {(first ks) (first vs)})))))

;; 66. Given two integers, write a function which returns the greatest common divisor.
(fn [n1 n2]
	(loop [d 1 ans 1]
		(if (or (> d n1) (> d n2))
			ans
			(if (and (= (mod n1 d) 0) (= (mod n2 d) 0))
				(recur (inc d) d)
				(recur (inc d) ans)))))

;; 81. Write a function which returns the intersection of two sets. The intersection is the sub-set of items that each set has in common.
(fn [s1 s2]
	(loop [lst s1 ans #{}]
		(if (empty? lst)
			ans
			(if (contains? s2 (first lst))
				(recur (rest lst) (into ans [(first lst)]))
				(recur (rest lst) ans)))))

;; 62. Given a side-effect free function f and an initial value x write a function which returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc.
(fn foo [f x]
	(cons x (lazy-seq (foo f (f x)))))

;; 107. It can be hard to follow in the abstract, so let's build a simple closure. Given a positive integer n, return a function (f x) which computes xn. Observe that the effect of this is to preserve the value of n for use outside the scope in which it is defined.
(fn [n]
	(fn [x]
		(reduce * (repeat n x))))

;; 99. Write a function which multiplies two numbers and returns the result as a sequence of its digits.
(fn [n1 n2]
	(loop [n (* n1 n2) lst []]
		(if (= n 0)
			lst
			(recur (quot n 10) (concat [(mod n 10)] lst)))))

;; 90. Write a function which calculates the Cartesian product of two sets.
(fn [l1 l2]
	(let [n (* (count l1) (count l2))]
		(into #{}
			(map vector
				(sort (take n (cycle l1))) (take n (cycle l2))))))

;; 63. Given a function f and a sequence s, write a function which returns a map. The keys should be the values of f applied to each item in s. The value at each key should be a vector of corresponding items in the order they appear in s.
(fn [f s]
	(loop [lst s ans (hash-map)]
		(if (empty? lst)
			ans
			(let [k (f (first lst))]
				(recur
					(rest lst)
					(assoc ans k (concat (get ans k) [(first lst)])))))))

;; 122. Convert a binary number, provided in the form of a string, to its numerical value.
#(Integer/parseInt % 2)

;; 88. Write a function which returns the symmetric difference of two sets. The symmetric difference is the set of items belonging to one but not both of the two sets.
(fn [s1 s2]
	(let [s (set (concat s1 s2))]
		(set (filter #(false? (and (contains? s1 %) (contains? s2 %))) s))))

;; 143. Create a function that computes the dot product of two sequences. You may assume that the vectors will have the same length.
#(apply + (map * %1 %2))

;; 135. Your friend Joe is always whining about Lisps using the prefix notation for math. Show him how you could easily write a function that does math using the infix notation. Is your favorite language that flexible, Joe? Write a function that accepts a variable length mathematical expression consisting of numbers and the operations +, -, *, and /. Assume a simple calculator that does not do precedence and instead just calculates left to right.
(fn [& args]
	(if (= (count args) 1)
		(first args)
		(let [[x f y & rst] args]
			(recur (cons (f x y) rst)))))

;; 118. Map is one of the core elements of a functional programming language. Given a function f and an input sequence s, return a lazy sequence of (f x) for each element x in s.
(fn map-2
	([f coll]
		(lazy-seq
			(when-let [s (seq coll)]
				(cons (f (first s)) (map-2 f (rest s)))))))

;; 95. Write a predicate which checks whether or not a given sequence represents a binary tree. Each node in the tree must have a value, a left child, and a right child.
(fn tree? [l]
	(cond
		(nil? l) true
		(coll? l)
		(and
			(= 3 (count l))
			((complement coll?) (first l))
			(tree? (second l))
			(tree? (last l)))
		:else false))

;; 44. Write a function which can rotate a sequence in either direction.
(defn rotate [n s]
	(let [l (mod n (count s))]
		(concat (drop l s) (take l s))))

;; 43. Write a function which reverses the interleave process into x number of subsequences.
(defn rev-interleave [s n]
	(loop [ans [] x 0]
		(if (= x n)
			ans
			(recur
				(concat
					ans
					[(map #(nth s %) (range x (count s) n))])
				(inc x)))))

;; 50. Write a function which takes a sequence consisting of items with different types and splits them up into a set of homogeneous sub-sequences.
(defn split-by-type [l]
	(vals (apply merge-with concat
		(for [x l]
			{(class x) [x]}
			))))

;; 157. Transform a sequence into a sequence of pairs containing the original elements along with their index.
(defn index-seq [s]
	(loop [l s i 0 ans []]
		(if (empty? l)
			ans
			(recur (rest l) (inc i) (concat ans [[(first l) i]])))))

;; 158. Write a function that accepts a curried function of unknown arity n. Return an equivalent function of n arguments.
(defn decurry [f]
	(fn [& l]
		(loop [func f args l]
			(if-not (fn? func)
				func
				(recur (func (first args)) (rest args))))))

;; 97. Pascal's Triangle
(defn pascal [n]
	(for [i (range 0 n)
		:let [f
			(fn binom [n k]
				(if	(or (= k 0) (= k n))
					1
					(+
						(binom (dec n) (dec k))
						(binom (dec n) k))))]]
			(f (dec n) i)))

;; 98. Equivalence Classes
(defn equiv [f d]
  (set
    (map set
         (vals (group-by f d)))))

;; 100. Least Common Multiple
(defn lcm [& nrs]
  (/
   (apply * nrs)
   ((fn gcd [v]
    (reduce
      (fn [n1 n2]
        (if (= n2 0)
          n1
          (recur n2 (rem n1 n2))))
     v))
    nrs)))

;; 77. Anagram finder
(defn anagram [v]
   (set
    (map
      set
      (filter
        #(> (count %) 1)
        (vals (group-by sort v))))))

;; 96. Beauty is Symmetry
(defn symmetric [[root left right]]
  (=
   ((fn mirror [[r le ri]]
      (if r
        [r (mirror ri) (mirror le)]
        r)) left)
   right))

;; 102. intoCamelCase
(defn camel-case [s]
  (reduce
   #(str %1 (clojure.string/capitalize %2))
   (clojure.string/split s #"-")))

;; 128. Recognize Playing Cards
(defn playing-cards [[suit rank]]
  {
   :suit
   (case suit
     \D :diamond
     \H :heart
     \C :club
     \S :spades)
   :rank
   (case rank
     \2 0
     \3 1
     \4 2
     \5 3
     \6 4
     \7 5
     \8 6
     \9 7
     \T 8
     \J 9
     \Q 10
     \K 11
     \A 12)
   })

;; 146. Trees into tables
(defn to-table [tree]
  (apply clojure.set/union
    (for [[k1 m] tree]
      (apply clojure.set/union
        (for [[k2 v] m]
          (hash-map
           [k1 k2] v))))))

;; 153. Pairwise Disjoint Sets
(defn disj? [sets]
  (every?
    empty?
   (for [s1 sets s2 sets :when (not= s1 s2)]
     (clojure.set/intersection s1 s2))))

;; 46. Flipping Out
(defn flip [f]
  (fn [& args]
    (apply f (reverse args))))

;; 56. Find Distinct Items
(defn distinct2 [lst]
  (reduce
   #(if (some #{%2} %1) %1 (conj %1 %2))
   []
   lst))

;; 58. Function Composition
(defn compose [& fns]
  (fn [& args]
    (reduce
     #(%2 %1)
     (apply (last fns) args)
     (reverse (butlast fns)))))

;; 59. Juxtaposition
(defn jux [& fns]
  (fn [& args]
    (for [f fns]
      (apply f args))))

;; 60. Sequence Reductions
(defn red
  ([f s lst] (red f (cons s lst)))
  ([f lst] (lazy-seq
    (map-indexed
     (fn [i x] (reduce f (take (inc i) lst)))
     lst))))

