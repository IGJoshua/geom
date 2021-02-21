(ns org.suskalo.geom.ops
  "Defines the core operations for doing geometric algebra."
  (:refer-clojure
   :rename {reverse core-reverse})
  (:require [clojure.core.matrix :as m]))

(defprotocol Multivector
  "Common operations on geometric algebra objects."
  (dot [v1 v2] "Computes the dot product between two multivectors.")
  (wedge [v1 v2] "Computes the wedge product between two multivectors.")
  (prod [v1 v2] "Computes the geometric product of two multivectors.")
  (scale [v s] "Scales a multivector by a factor.")
  (reverse [v]
    "Computes the reverse of a given multivector.
    This is often represented in texts with the dagger operator.")
  (grade [v] "The highest grade of this multivector.")

  (component [v grade] "Fetches the object of given `grade` from the multivector.")
  (with-component [v grade val] "Returns a new multivector with `val` for the given `grade`.")
  (zero [v] "Creates a zero vector of this grade."))

(defn vector-inverse
  "Given a multivector with only a grade-1 portion, constructs the inverse."
  [v]
  (scale v (/ (component (prod v v) 0))))

(defn vector-reflect
  "Reflects vector `v` across `w`."
  [v w]
  (prod (prod (vector-inverse w) v) w))

(defn normalise-component
  "Normalises the grade-`c` component."
  [v c]
  (let [component (component v c)
        factor (cond-> component
                 (m/array? component) m/magnitude
                 :finally /)]
    (scale v factor)))

(defn vector-normalise
  "Normalises a vector."
  [v]
  (normalise-component v 1))

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
