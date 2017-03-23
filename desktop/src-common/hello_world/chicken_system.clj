(ns hello-world.chicken-system
  (:gen-class))

(def direction-id-map-delta
  {:up    [ 0  1]
   :right [ 1  0]
   :down  [ 0 -1]
   :left  [-1  0]})

(defn random-direction []
  ((rand-nth [:up :right :down :left]) direction-id-map-delta))

(def dimension 50)

(defn wrap
  ""
  [x]
  (cond
    (>= x dimension) 0
    (<  x 0) (dec dimension)
    :else x))

(defn get-tile
  [grid [i j]]
  (get-in grid [i j]))

(defn is-tile-free?
  [grid [i j]]
  (= false (deref (get-tile grid [i j]))))

(defn is-tile-busy?
  [grid [i j]]
  (= true (deref (get-tile grid [i j]))))

(defn make-grid
  [dimension f]
  (apply vector 
         (map (fn [_] 
                (apply vector (map (fn [_] (f)) 
                                   (range dimension)))) 
              (range dimension))))

(def dead-grid  (make-grid dimension #(ref false)))
(def alive-grid (make-grid dimension #(ref false)))
;; Ants
(defstruct ant-struct :x :y :holding)

(defn create-ant
  [[i j]] 
  (ref-set (get-tile alive-grid [i j]) true)
  (ref (struct ant-struct j i nil)))

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
                    tile-busy (is-tile-busy? grid [i j])]
                (if tile-busy
                  (recur num-ants-left new-ants)
                  (recur (dec num-ants-left) (conj new-ants (create-ant [i j])))))))))

(def num-ants 1)
(def ants (create-ants num-ants))

;; Bodies
(defn create-body
  [[i j]]
  (ref-set (get-tile dead-grid [i j]) true)
  (ref (struct ant-struct j i)))

(defn create-bodies
  [num-bodies]
  (dosync (loop [num-bodies-left num-bodies
                 new-bodies []]
            (if (zero? num-bodies-left)
              new-bodies
              (let [i (rand-int dimension)
                    j (rand-int dimension)
                    grid dead-grid
                    tile-busy (is-tile-busy? grid [i j])]
                (if tile-busy
                  (recur num-bodies-left new-bodies)
                  (recur (dec num-bodies-left) (conj new-bodies (create-body [i j])))))))))

(def num-bodies 30)
(def bodies (create-bodies num-bodies))

(defn move-ant
  ""
  [ant [dx dy]]
  (dosync
   (let [x (:x (deref ant))
         y (:y (deref ant))
         new-x (wrap (+ x dx))
         new-y (wrap (+ y dy))]
     (if (is-tile-free? alive-grid [new-y new-x])
       (do
         (ref-set (get-tile alive-grid [y x]) false)
         (ref-set (get-tile alive-grid [new-y new-x]) true)
         (ref-set ant (struct ant-struct new-x new-y)))
       ant))))

(defn loop-ants
  ""
  []
  ;; (move-ant (first ants) [1 0])
  (loop [ant (first ants)
         left-ants (rest ants)]
    (if (nil? ant)
      nil
      (do
        (move-ant ant (random-direction))
        (recur (first left-ants) (rest left-ants)))))
  )



