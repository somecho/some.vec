(ns some.vec
  "A collection of helper functions that work on vectors,
  both in the sense of a collection and in the directional sense."
  (:refer-clojure :exclude [+ - * /]))

(declare from-polar
         length
         mat-trans
         pad-end
         pad-start
         range-fill
         restv
         set-length)

(defn +
  "Adds two vectors of the same dimension together.
  If the second argument is a scalar, it gets added to
  all components of the first argument.

  ** Example **

  ```clojure
  (some.vec/+ [1 1][1 1])
  ;; (2 2)

  (some.vec/+ [1 1] 2)
  ;; (3 3)
  ```"
  [a b]
  (if (coll? b)
    (map clojure.core/+ a b)
    (+ a (repeat (count a) b))))

(defn -
  "Substracts 2 vectors of same dimension. If the second argument is a
  scalar, all components of the first argument gets subtracted by it.

  ** Example **
  ```clojure
  (some.vec/- [2 2][1 1])
  ;; (1 1)

  (some.vec/- [5 5] 1)
  ;; (4 4)
  ```"
  [a b]
  (if (coll? b)
    (map clojure.core/- a b)
    (- a (repeat (count a) b))))

(defn
  * "Component-wise multiplication of two n-dimensional vectors.
  If the second argument is a scalar, all components of the first
  argument gets multiplied by it.

  ** Example **
  ```clojure
  (some.vec/* [2 2][2 2])
  ;; (4 4)

  (some.vec/* [2 2] 4)
  ;; (8 8)
  ```"
  [a b]
  (if (coll? b)
    (map clojure.core/* a b)
    (* a (repeat (count a) b))))

(defn /
  "Divides vector a by vector b. Both vectors should be of the same
  dimension. If the second argument is a scalar, all components
  of the first argument gets divided by it.

  ** Example **

  ```clojure
  (some.vec// [2 2][2 2])
  ;; (1 1)

  (some.vec// [2 2] 2)
  ;; (1 1)
  ```"
  [a b]
  (if (coll? b)
    (map clojure.core// a b)
    (/ a (repeat (count a) b))))

(defn angle
  "Returns the angle of a 2D vector in radians."
  [v] (let [[x y] v] (Math/atan2 y x)))

(defn angle-between
  "Returns the angle between two 2D vectors, calculated using the
  difference between their angles."
  [v1 v2] (clojure.core/- (angle v2) (angle v1)))

(defn average
  "Returns the average of a vector."
  [v] (clojure.core// (reduce clojure.core/+ v) (count v)))

(defn distance
  "Calculates the distance between two vectors of same dimension."
  [v1 v2] (length (- v1 v2)))

(defn dot
  "Returns the dot product of two n-dimensional vectors."
  [v1 v2] (reduce clojure.core/+ (* v1 v2)))

(defn dx->x
  "Treats each component of a vector as an interval and
  creates a running sum of the intervals.

  For example:
  [1 1 1] -> [0 1 2 3]
  [1 2 3] -> [0 1 3 6]"
  ([v] (dx->x v 0))
  ([v start]
   (cons start (->> v
                    count
                    range
                    (map inc)
                    (map #(take % v))
                    (map #(clojure.core/+ (reduce clojure.core/+ %) start))))))

(defn from-angle
  "Returns a 2D unit vector with given angle"
  [theta] (from-polar theta 1))

(defn from-polar
  "Returns a 2D vector from polar coordinates"
  [theta radius]
  [(clojure.core/* radius (Math/cos theta))
   (clojure.core/* radius (Math/sin theta))])

(defn indexed
  "pairs each element in a collection as
   an indexed tuple and return as vector"
  [coll]
  (mat-trans [(range (count coll)) coll]))

(defn length
  "Returns the length of a vector of any dimension."
  [v] (->> (map clojure.core/* v v) (reduce clojure.core/+) (Math/sqrt)))

(defn lerpn [a b t] (clojure.core/+ (clojure.core/* (clojure.core/- 1 t) a) (clojure.core/* t b)))

(defn lerp
  "Lerps between two vectors"
  [v1 v2 t] (letfn [(f [a b] (lerpn a b t))] (map f v1 v2)))

(defn limit
  "If vector v is bigger than limit l, vector gets scaled to l."
  [v l] (if (> (length v) l) (set-length v l) v))

(defn mat-trans
  "Rotates an ND matrix (vec of vecs)"
  [mat]
  (apply map vector mat))

(defn normalize
  "Returns a vector of any dimension normalized"
  [v] (map #(clojure.core// % (length v)) v))

(defn normalize-range
  "Divides every element in vector by total sum"
  [v] (map #(clojure.core// % (reduce clojure.core/+ v)) v))
(defn pad
  "pads start and end of vector with value and pad size"
  ([vector value]
   (-> vector (pad-start value) (pad-end value)))
  ([vector value pad-size]
   (-> vector (pad-start value pad-size) (pad-end value pad-size))))

(defn pad-end
  "pads end of vector with value and pad size"
  ([vector value]
   (vec (concat vector (list value))))
  ([vector value pad-size]
   (vec (concat vector (repeat pad-size value)))))

(defn pad-start
  "pads start of vector with value and pad size"
  ([vector value]
   (vec (concat (list value) vector)))
  ([vector value pad-size]
   (vec (concat (repeat pad-size value) vector))))

(defn pad-wrap
  "pads start of vector with end and end of vector with start"
  [vector]
  (let [a (first vector)
        b (last vector)]
    (-> vector (pad-start b) (pad-end a))))

(defn random
  "Returns a random vector of N dimension. Values between 0 and 1.
  An optional second argument can be given as another RNG function."
  ([N] (vec (repeatedly N rand)))
  ([N f] (vec (repeatedly N f))))

(defn random10
  "Returns a vector of size N of random 1s and 0s.
  An optional second argument can be given as another RNG function."
  ([N] (random10 N rand))
  ([N f] (repeatedly N #(Math/floor (clojure.core/* (f) 2)))))

(defn random-polar
  "Returns a 2D vector from a random angle and radius between 0 and 1.
  An optional second argument can be given as another RNG function."
  ([] (random-polar rand))
  ([f] (from-polar (clojure.core/* (f) Math/PI 2) (f))))

(defn random-replace
  "Replace random element in vector with a value"
  ([vec value] (random-replace vec value rand))
  ([vec value f]
   (let [i (-> (count vec)
               (clojure.core/* (f))
               (Math/floor))]
     (range-fill vec i i value))))

(defn range-fill
  "Fill a range of elements in vector with value.

  ** Example **

  ```clojure
  (some.vec/range-fill [0 1 2 3 4 5] 2 4 69)
  ;; [0 1 69 69 69 5]
  ```"
  [vec start stop value]
  (let [new-vec (assoc vec start value)]
    (if (== (clojure.core/- stop start) 0)
      new-vec
      (range-fill new-vec (inc start) stop value))))

(defn range-replace
  "replace elements of vector with another set at given position"
  [vec value position]
  (let [new-vec (assoc vec position (first value))
        rest (restv value)]
    (if (empty? rest)
      new-vec
      (range-replace new-vec rest (inc position)))))

(defn restv
  "returns the rest of a collection as a vector"
  [col] (vec (rest col)))

(defn rotate
  "rotates a 2D vector by an angle"
  [v theta] (from-polar (clojure.core/+ (angle v) theta) (length v)))

(defn set-length
  "Returns the vector v with given length l."
  [v l] (* (normalize v) l))

(defn select
  "Returns the selection of a vector from index a to and including index b."
  [v a b]
  (->> v (drop a) (drop-last (clojure.core/- (count v) b 1))))

(defn split
  "Return two collections split from a vector with i as the index
  starting the second group."
  [v i]
  [(drop-last (clojure.core/- (count v) i) v) (drop i v)])
