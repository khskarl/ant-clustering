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
  (= false (:is-busy (deref (get-tile grid [i j])))))

(defn is-tile-busy?
  [grid [i j]]
  (:is-busy (deref (get-tile grid [i j]))))

(defn make-grid
  [dimension f]
  (apply vector 
         (map (fn [_] 
                (apply vector (map (fn [_] (f)) 
                                   (range dimension)))) 
              (range dimension))))

(defstruct tile-struct :is-busy :occupied-by)

(def dead-grid  (make-grid dimension #(ref (struct tile-struct false nil))))
(def alive-grid (make-grid dimension #(ref (struct tile-struct false nil))))
;; Ants
(defstruct ant-struct :x :y :holding)

(defn create-ant
  [[i j]]
  (let [tile-ref (get-tile alive-grid [i j])
        tile     (deref tile-ref)
        ant-ref (ref (struct ant-struct j i nil))] 
    (ref-set tile-ref (-> tile
                          (assoc :is-busy true)
                          (assoc :occupied-by ant-ref)))
    ant-ref))

(defn create-ants
  ""
  [num-ants]
  (dosync (loop [num-ants-left num-ants
                 new-ants []]
            (if (zero? num-ants-left)
              new-ants
              (let [i (rand-int dimension)
                    j (rand-int dimension)
                    tile-busy (is-tile-busy? alive-grid [i j])]
                (if tile-busy
                  (recur num-ants-left new-ants)
                  (recur (dec num-ants-left)
                         (conj new-ants (create-ant [i j])))))))))

(def num-ants 100)
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

(def num-bodies 10)
(def bodies (create-bodies num-bodies))

(defn move-ant!
  [ant [dx dy]]
  (dosync
   (let [x (:x (deref ant))
         y (:y (deref ant))
         new-x (wrap (+ x dx))
         new-y (wrap (+ y dy))]
     (if (is-tile-free? alive-grid [new-y new-x])
       (let [tile-ref (get-tile alive-grid [y x])
             tile     (deref tile-ref)
             next-tile-ref (get-tile alive-grid [new-y new-x])
             next-tile     (deref next-tile-ref)]
         (ref-set tile-ref      (assoc tile      :is-busy false))
         (ref-set next-tile-ref (assoc next-tile :is-busy true))
         (ref-set ant (struct ant-struct new-x new-y)))
       ant))))

(defn pick-body
  [ant-ref body-ref]
  (let [ant (deref ant-ref)
        body (deref body-ref)]
    (dosync
     (ref-set body-ref (assoc body :x dimension :y dimension))
     (ref-set ant (assoc ant :holding body-ref))))
  )

(defn drop-body
  [ant-ref]
  (let [ant (deref ant-ref)]
    ""))

(defn loop-ant
  [ant-ref]
  (move-ant! ant-ref (random-direction)))

(defn loop-ants
  []
  (run! loop-ant ants))

;; (let [i (:y (deref ant-ref))
;;       j (:x (deref ant-ref))
;;       has-body-below (is-tile-busy? dead-grid [i j])]
;;   (if (and (> (rand) 0.5) has-body-below)
;;     (get-body ant (get-body-from-tile [i j]))))
