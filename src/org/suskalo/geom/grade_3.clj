(ns org.suskalo.geom.grade-3
  (:require
   [clojure.core.matrix :as m]
   [org.suskalo.geom.ops :as ops]
   [org.suskalo.geom.protocols :as proto])
  (:refer-clojure
   :rename {vector core-vector}))

(def ^:private grade 3)

(deftype Multivec3 [s v b t]
  proto/Multivector
  (dot [v1 v2]
    (let [a s
          [f g h] (m/eseq b)
          [b c d] (m/eseq v)
          i t
          j (.-s v2)
          [n o p] (m/eseq (.-b v2))
          [k l m] (m/eseq (.-v v2))
          q (.-t v2)]
      (Multivec3.
       (+ (* a j)
          (* b k)
          (* c l)
          (- (* d m)
             (* f n)
             (* g o)
             (* h p)
             (* i q)))
       (m/array [(+ (* a k)
                    (* b j)
                    (* d p)
                    (- (* f l)
                       (* c n)
                       (* g q)
                       (* h m)
                       (* i o)))
                 (+ (* a l)
                    (* b n)
                    (* g m)
                    (- (* c j)
                       (* d o)
                       (* f k)
                       (* h q)
                       (* i p)))
                 (+ (* a m)
                    (* c o)
                    (* d j)
                    (- (* h k)
                       (* b p)
                       (* f q)
                       (* g l)
                       (* i n)))])
       (m/array [(+ (* a n)
                    (* d q)
                    (* f j)
                    (* i m))
                 (+ (* a o)
                    (* b q)
                    (* g j)
                    (* i k))
                 (+ (* a p)
                    (* c q)
                    (* h j)
                    (* i l))])
       (+ (* a q)
          (* i j)))))
  (wedge [v1 v2]
    (let [a s
          [f g h] (m/eseq b)
          [b c d] (m/eseq v)
          i t
          j (.-s v2)
          [n o p] (m/eseq (.-b v2))
          [k l m] (m/eseq (.-v v2))
          q (.-t v2)]
      (Multivec3.
       (* a j)
       (m/array [(+ (* a k)
                    (* b j))
                 (+ (* a l)
                    (* c j))
                 (+ (* a m)
                    (* d j))])
       (m/array [(+ (* a n)
                    (* b l)
                    (* c k -1.0)
                    (* f j))
                 (+ (* a o)
                    (* c m)
                    (* d l -1.0)
                    (* g j))
                 (+ (* a p)
                    (* b m -1.0)
                    (* d k)
                    (* h j))])
       (+ (* a q)
          (* b o)
          (* c p)
          (* d n)
          (* f m)
          (* g k)
          (* h l)
          (* i j)))))
  (prod [_ v2]
    (let [a s
          [f g h] (m/eseq b)
          [b c d] (m/eseq v)
          i t
          j (.-s v2)
          [n o p] (m/eseq (.-b v2))
          [k l m] (m/eseq (.-v v2))
          q (.-t v2)]
      (Multivec3.
       (+ (* a j)
          (* b k)
          (* c l)
          (- (* d m)
             (* f n)
             (* g o)
             (* h p)
             (* i q)))
       (m/array [(+ (* a k)
                    (* b j)
                    (* d p)
                    (- (* f l)
                       (* c n)
                       (* g q)
                       (* h m)
                       (* i o)))
                 (+ (* a l)
                    (* b n)
                    (* c j)
                    (- (* g m)
                       (* d o)
                       (* f k)
                       (* h q)
                       (* i p)))
                 (+ (* a m)
                    (* c o)
                    (* d j)
                    (- (* h k)
                       (* b p)
                       (* f q)
                       (* g l)
                       (* i n)))])
       (m/array [(+ (* a n)
                    (* b l)
                    (* d q)
                    (* f j)
                    (* h o)
                    (- (* i m)
                       (* g p)
                       (* c k)))
                 (+ (* a o)
                    (* b q)
                    (* c m)
                    (* f p)
                    (* g j)
                    (- (* i k)
                       (* h n)
                       (* d l)))
                 (+ (* a p)
                    (* c q)
                    (* d k)
                    (* g n)
                    (* h j)
                    (- (* i l)
                       (* f o)
                       (* b m)))])
       (+ (* a q)
          (* b o)
          (* c p)
          (* d n)
          (* f m)
          (* g k)
          (* h l)
          (* i j)))))
  (add [_ v2]
    (Multivec3. (+ s (.-s v2))
                (m/add v (.-v v2))
                (m/add b (.-b v2))
                (+ t (.-t v2))))
  (scale [_ scale]
    (Multivec3. (* s scale) (m/mul v scale) (m/mul b scale) (* t scale)))
  (reverse [_]
    (Multivec3. s v (m/scale b -1.0) (m/mul t -1.0)))
  (grade [_] grade)

  (component [_ grade]
    (case grade
      0 s
      1 v
      2 b
      3 t))
  (with-component [_ grade val]
    (case grade
      0 (Multivec3. val v b t)
      1 (Multivec3. s val b t)
      2 (Multivec3. s v val t)
      3 (Multivec3. s v b val)))
  (zero [_]
    (Multivec3. 0 [0 0 0] [0 0 0] 0))

  (inverse [v]
    (throw (ex-info "Attempted a non-trivial inverse of a grade-3 multivec."
                    {:multivec v :unimplemented true}))))

(defmethod print-method Multivec3 [v ^java.io.Writer w]
  (.write w (str "#org.suskalo.geom.grade-3.Multivec3 ["
                 (pr-str (.-s v)) " "
                 (pr-str (m/as-vector (.-v v))) " "
                 (pr-str (.-b v)) " "
                 (pr-str (.-t v))
                 "]")))

(defn multivec
  "Constructs a multivector from the given components."
  [scalar vector bivector trivector]
  (Multivec3.
   scalar
   (if (m/array? vector)
     vector
     (m/array vector))
   (if (m/array? bivector)
     bivector
     (m/array bivector))
   trivector))

(defn scalar
  "Constructs a multivector with only a scalar component."
  [scalar]
  (multivec scalar [0 0 0] [0 0 0] 0))

(defn vector
  "Constructs a multivector with only a grade-1 component."
  ([vector]
   (multivec 0 vector [0 0 0] 0))
  ([x y z]
   (multivec 0 (m/array [x y z]) [0 0 0] 0)))

(defn bivector
  "Constructs a multivector with only a grade-2 component."
  ([bivector]
   (multivec 0 [0 0 0] bivector 0))
  ([xy yz zx]
   (multivec 0 [0 0 0] [xy yz zx] 0)))

(defn trivector
  "Constructs a multivector with only a grade-3 component."
  [trivector]
  (multivec 0 [0 0 0] [0 0 0] trivector))

(defn rotate-x
  "Construct a rotor to rotate around the x axis."
  [radians]
  (ops/rotor-from-unit (bivector [0 1 0]) radians))

(defn rotate-y
  "Construct a rotor to rotate around the y axis."
  [radians]
  (ops/rotor-from-unit (bivector [0 0 1]) radians))

(defn rotate-z
  "Construct a rotor to rotate around the z axis."
  [radians]
  (ops/rotor-from-unit (bivector [1 0 0]) radians))

(defn cross
  "Construct a vector perpendicular to both inputs."
  [v1 v2]
  (ops/prod (trivector -1) (ops/wedge v1 v2)))

(defn euler-angles
  "Constructs a rotor for performing an Euler angle rotation.
  Uses ZXZ rotation."
  [alpha beta gamma]
    (ops/prod (rotate-z gamma) (ops/prod (rotate-x beta) (rotate-z alpha))))

(defn cardinal-angles
  "Constructs a rotor for performing a Cardanian angle rotation.
  Uses XYZ rotation."
  [pitch yaw roll]
  (ops/prod (rotate-z roll) (ops/prod (rotate-y yaw) (rotate-x pitch))))
