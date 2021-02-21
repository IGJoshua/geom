(ns org.suskalo.geom.protocols
  (:refer-clojure
   :rename {reverse core-reverse}))

(defprotocol Multivector
  "Common operations on geometric algebra objects."
  (dot [v1 v2] "Computes the dot product between two multivectors.")
  (wedge [v1 v2] "Computes the wedge product between two multivectors.")
  (prod [v1 v2] "Computes the geometric product of two multivectors.")
  (add [v1 v2] "Computes the sum of two multivectors.")
  (scale [v s] "Scales a multivector by a factor.")
  (reverse [v]
    "Computes the reverse of a given multivector.
    This is often represented in texts with the dagger operator.")
  (grade [v] "The highest grade of this multivector.")

  (component [v grade] "Fetches the object of given `grade` from the multivector.")
  (with-component [v grade val] "Returns a new multivector with `val` for the given `grade`.")
  (zero [v] "Creates a zero vector of this grade.")

  (inverse [v]
    "Computes the inverse, if one exists, of the given multivector.
    Implementation is not required for inverses to be computed, but a limited
    number of multivectors will have an inverse if it is not.

    Throws an ex-info with the multivector in the `:multivec` key of the data if
    no inverse can be computed. An additional key of `:unimplemented` may be
    truthy if the reason for the lack of inverse is due to a lack of
    implementaiton."))
