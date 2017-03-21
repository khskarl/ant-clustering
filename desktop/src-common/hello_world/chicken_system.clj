(ns hello-world.chicken-system)


(def dimension 3)

(def dead-grid (make-grid dimension))
(def alive-grid (make-grid dimension))

(defstruct ant :x :y)
(def ants (create-ants 1))

(defn get-tile
  [grid [y x]]
  (get-in grid [y x]))

(defn make-grid
  [dimension]
  (apply vector 
         (map (fn [_] 
                (apply vector (map (fn [_] (ref false)) 
                                   (range dimension)))) 
              (range dimension))))

(defn create-ant
  [[i j]] 
  (ref-set (get-tile alive-grid [i j]) true)
  (ref (struct ant j i)))

(defn create-ants
  ""
  [num-ants]
  (dosync (loop [num-ants-left num-ants
                 new-ants []]
            (if (zero? num-ants-left)
              new-ants
              (let [i (rand-int dimension)
                    j (rand-int dimension)
                    grid alive-grid
                    is-tile-busy (deref (get-tile grid [i j]))]
                (if is-tile-busy
                  (recur num-ants-left new-ants)
                  (recur (dec num-ants-left) (conj new-ants (create-ant [i j])))))))))

(defn create-bodies
  [num-bodies]
  "I do nothing :D")

(defn move-ant
  ""
  [ant [dx dy]]
  (dosync
   (let [x (:x ant)
         y (:y ant)
         new-x (+ x dx)
         new-y (+ y dy)]
     (ref-set (get-tile alive-grid [y x] false))
     (ref-set (get-tile alive-grid [new-y new-x] true))
     (ref-set ant (struct new-x new-y))
     )
   ))


(defn dec-tile [x]
  (let [new-x (dec x)]
    (if (< new-x 0)
      (- dimension 1)
      new-x)))

(defn inc-tile [x]
  (let [new-x (inc x)]
    (if (>= new-x dimension)
      0
      new-x)))

(defn random-direction [] (rand-nth [:down :up :left :right]))


