(ns org.suskalo.geom.grade-2
  (:require
   [clojure.core.matrix :as m]
   [org.suskalo.geom.ops :as ops]
   [org.suskalo.geom.protocols :as proto])
  (:refer-clojure
   :rename {vector core-vector}))

(def ^:private grade 2)

(deftype Multivec2 [s v b]
  proto/Multivector
  (dot [v1 v2]
    (let [a s
          d b
          [b c] (m/eseq v)
          x (.-s v2)
          w (.-b v2)
          [y z] (m/eseq (.-v v2))]
      (Multivec2.
       (+ (* a x)
          (* b y)
          (* c z)
          (* d w -1.0))
       (m/array [(+ (* a y)
                    (* b x)
                    (* c w -1.0)
                    (* d z))
                 (+ (* a z)
                    (* b w)
                    (* c x)
                    (* d y -1.0))])
       (+ (* a w)
          (* d x)))))
  (wedge [_ v2]
    (let [a s
          d b
          [b c] (m/eseq v)
          x (.-s v2)
          w (.-b v2)
          [y z] (m/eseq (.-v v2))]
      (Multivec2.
       (* a x)
       (m/array [(+ (* a y)
                    (* b x))
                 (+ (* a z)
                    (* c x))])
       (+ (* a w)
          (* b z)
          (* c y -1.0)
          (* d x)))))
  (prod [_ v2]
    (let [a s
          d b
          [b c] (seq v)
          x (.-s v2)
          [y z] (seq (.-v v2))
          w (.-b v2)]
      (Multivec2.
       (+ (* a x)
          (* b y)
          (* c z)
          (* d w -1))
       (m/array [(+ (* a y)
                    (* b x)
                    (* c w -1)
                    (* d z))
                 (+ (* a z)
                    (* b w)
                    (* c x)
                    (* d y -1))])
       (+ (* a w)
          (* b z)
          (* c y -1)
          (* d x)))))
  (add [_ v2]
    (Multivec2. (+ s (.-s v2))
                (m/add v (.-v v2))
                (+ b (.-b v2))))
  (scale [_ scale]
    (Multivec2. (* s scale) (m/mul v scale) (* b scale)))
  (reverse [_]
    (Multivec2. s v (- b)))
  (grade [_] grade)

  (component [_ grade]
    (case grade
      0 s
      1 v
      2 b))
  (with-component [_ grade val]
    (case grade
      0 (Multivec2. val v b)
      1 (Multivec2. s val b)
      2 (Multivec2. s v val)))
  (zero [_]
    (Multivec2. 0 [0 0] 0))

  (inverse [v]
    (throw (ex-info "Attempted a non-trivial inverse of a grade-2 multivec."
                    {:multivec v :unimplemented true}))))

(defmethod print-method Multivec2 [v ^java.io.Writer w]
  (.write w (str "#org.suskalo.geom.grade-2.Multivec2 ["
                 (pr-str (.-s v)) " "
                 (pr-str (m/as-vector (.-v v))) " "
                 (pr-str (.-b v))
                 "]")))

(defn multivec
  "Constructs a multivector from the given components."
  [scalar vector bivector]
  (Multivec2.
   scalar
   (if (m/array? vector)
     vector
     (m/array vector))
   bivector))

(defn scalar
  "Constructs a multivector with only a scalar component."
  [scalar]
  (multivec scalar [0 0] 0))

(defn vector
  "Constructs a multivector with only a grade-1 component."
  ([vector]
   (multivec 0 vector 0))
  ([x y]
   (multivec 0 (m/array [x y]) 0)))

(defn bivector
  "Constructs a multivector with only a grade-2 component."
  [bivector]
  (multivec 0 [0 0] bivector))

(defn rotor
  "Constructs a rotor to rotate vectors by `radians`."
  [radians]
  (ops/rotor-from-unit (bivector 1) radians))
