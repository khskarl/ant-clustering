(ns hello-world.chicken-system
  (:gen-class)
  (:require clojure.pprint))

(def direction-id-map-delta
  {:up    [ 0  1]
   :right [ 1  0]
   :down  [ 0 -1]
   :left  [-1  0]})

(defn random-direction []
  ((rand-nth [:up :right :down :left]) direction-id-map-delta))

(def dimension 30)

(defn wrap
  ""
  [x]
  (cond
    (>= x dimension) 0
    (<  x 0) (dec dimension)
    :else x))

(defn get-tile-ref
  [grid [i j]]
  (get-in grid [i j]))

(defn is-tile-free?
  [grid [i j]]
  (= false (:is-busy (deref (get-tile-ref grid [i j])))))

(defn is-tile-busy?
  [grid [i j]]
  (:is-busy (deref (get-tile-ref grid [i j]))))

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

(defn get-body-ref-in-pos
  [[i j]]
  (println "GETTING BODY :D")
  (:occupied-by (deref (get-tile-ref dead-grid [i j]))))


;; Ants
(defstruct ant-struct :x :y :carrying)

(defn is-ant-carrying?
  [ant]
  (not (nil? (:carrying ant))))


(defn create-ant
  [[i j]]
  (let [tile-ref (get-tile-ref alive-grid [i j])
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

(def num-ants 1)
(def ants (create-ants num-ants))

;; Bodies
(defn create-body
  [[i j]]
  (let [tile-ref (get-tile-ref dead-grid [i j])
        tile     (deref tile-ref)
        body-ref (ref (struct ant-struct j i))] 
    (ref-set tile-ref (-> tile
                          (assoc :is-busy true)
                          (assoc :occupied-by body-ref)))
    body-ref))

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

(def num-bodies 40)
(def bodies (create-bodies num-bodies))

(defn has-body-below-ant?
  [ant]
  (let [i (:y ant)
        j (:x ant)] 
    (is-tile-busy? dead-grid [i j])))

(defn chance-to-pick
  []
  0)

(defn chance-to-drop
  []
  0)

(defn move-ant!
  [ant [dx dy]]
  (dosync
   (let [x (:x (deref ant))
         y (:y (deref ant))
         new-x (wrap (+ x dx))
         new-y (wrap (+ y dy))]
     (if (is-tile-free? alive-grid [new-y new-x])
       (let [tile-ref (get-tile-ref alive-grid [y x])
             tile     (deref tile-ref)
             next-tile-ref (get-tile-ref alive-grid [new-y new-x])
             next-tile     (deref next-tile-ref)]
         (ref-set tile-ref      (assoc tile      :is-busy false))
         (ref-set next-tile-ref (assoc next-tile :is-busy true))
         (ref-set ant (struct ant-struct new-x new-y)))
       ant))))

(defn pick-body!
  [ant-ref body-ref]
  (let [ant (deref ant-ref)
        body (deref body-ref)
        i (:y body)
        j (:x body)
        tile-ref (get-tile-ref dead-grid [i j])
        tile (deref tile-ref)]
    (println ant "picking" body-ref)
    (dosync
     (alter tile-ref assoc :occupied-by nil)
     (alter tile-ref assoc :is-busy false)
     (alter body-ref assoc :x dimension)
     (alter body-ref assoc :y dimension)
     (alter ant-ref  assoc :carrying body-ref))))

(defn pick-body-below!
  ""
  [ant-ref]
  (let [i (:y (deref ant-ref))
        j (:x (deref ant-ref))
        body-below (get-body-ref-in-pos [i j])]
    (pick-body! ant-ref body-below)))


(defn drop-body!
  [ant-ref]
  (let [ant (deref ant-ref)]
    ""))

(defn loop-ant
  [ant-ref]
  (let [i (:y (deref ant-ref))
        j (:x (deref ant-ref))
        has-body-below (has-body-below-ant? (deref ant-ref))
        is-carrying (is-ant-carrying? (deref ant-ref))]
    (if (and (not is-carrying)
             has-body-below) 
      (pick-body-below! ant-ref) 
      ;; (pick-body! ant (get-body-from-tile [i j]))
      
      ))
  ;; (move-ant! ant-ref (random-direction))
  (move-ant! ant-ref [0 1])
  )

(defn loop-ants
  []
  (run! loop-ant ants))

