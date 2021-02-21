(ns org.suskalo.geom.ops
  "Defines the core operations for doing geometric algebra."
  (:require
   [clojure.core.matrix :as m]
   [org.suskalo.geom.protocols :as proto]
   [org.suskalo.geom.ops :as ops])
  (:refer-clojure
   :rename {reverse core-reverse}))

(defmulti dot
  "Computes the dot product between two multivectors."
  (fn [v1 v2]
    [(class v1) (class v2)]))

(defmethod dot :default
  [v1 v2]
  (proto/dot v1 v2))

(defmulti wedge
  "Computes the wedge product between two multivectors."
  (fn [v1 v2]
    [(class v1) (class v2)]))

(defmethod wedge :default
  [v1 v2]
  (proto/wedge v1 v2))

(defmulti prod
  "Computes the geometric product of two multivectors."
  (fn [v1 v2]
    [(class v1) (class v2)]))

(defmethod prod :default
  [v1 v2]
  (proto/prod v1 v2))

(defmulti add
  "Computes the sum of two multivectors."
  (fn [v1 v2]
    [(class v1) (class v2)]))

(defmethod add :default
  [v1 v2]
  (proto/add v1 v2))

(defn scale
  "Scales a multivector by a factor."
  [v s]
  (proto/scale v s))

(defn sub
  "Computes the difference between two multivectors."
  [v1 v2]
  (add v1 (scale v2 -1.0)))

(defn reverse
  "Computes the reverse of a given multivector.
  This is often represented in texts with the dagger operator."
  [v]
  (proto/reverse v))

(defn grade
  "The highest grade of this multivector."
  [v]
  (proto/grade v))

(defn component
  "Fetches the object of given `grade` from the multivector."
  [v grade]
  (proto/component v grade))

(defn with-component
  "Returns a new multivector with `val` for the given `grade`."
  [v grade val]
  (proto/with-component v grade val))

(defn zero
  "Creates a zero vector of this grade."
  [v]
  (proto/zero v))

(defn select-component
  "Selects only the component with the given `grade`."
  [v grade]
  (with-component (zero v) grade (component v grade)))

(defn magnitude-squared
  "The sum of squares of all components of the multivector."
  [v]
  (transduce (comp (map (partial component v))
                   (map (fn [n-or-v]
                          (if (m/array? n-or-v)
                            (m/magnitude-squared n-or-v)
                            (* n-or-v n-or-v)))))
             + 0
             (range (inc (grade v)))))

(defn magnitude
  "The length of the full multivector."
  [v]
  (Math/sqrt
   (magnitude-squared v)))

(defn normalise
  "Normalises the entire multivector to a magnitude of 1."
  [v]
  (scale v (/ (magnitude v))))

(defn inverse
  "Fetches the inverse of a multivector."
  [v]
  ;; FIXME(Joshua): Ensure that all invertible vectors are correctly inverted
  (let [divisor (prod v (reverse v))]
    (doseq [i (map inc (range (grade divisor)))
            :let [to-test (component divisor i)]]
      (if (m/array? to-test)
        (assert (m/zero-matrix? to-test) "The vector is invertible")
        (assert (zero? to-test) "The vector is invertible")))
    (assert (not (zero? (component divisor 0))) "The vector is invertible")
    (scale (reverse v) (/ (component divisor 0)))))

(defn reflect
  "Reflects multivector `v` across a plane perpendicular to vector `w`."
  [v w]
  (scale (prod (prod (inverse w) v) w) -1.0))

(defn div
  "Computes the quotient of two multivectors."
  [v1 v2]
  (prod v1 (inverse v2)))

(defn rotor-from-unit
  "Constructs a rotor which rotates around the given unit multivector."
  [unit-plane radians]
  (with-component
    (scale unit-plane (- (Math/sin (/ radians 2))))
    0
    (Math/cos (/ radians 2))))

(defn rotor
  "Constructs a rotor from the given plane of rotation."
  [plane-of-rotation radians]
  (rotor-from-unit (normalise (with-component plane-of-rotation 0 0)) radians))

(defn rotate
  "Applies a rotation to a vector."
  [v rotor]
  (prod (prod rotor v) (reverse rotor)))
