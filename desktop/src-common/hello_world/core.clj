(ns hello-world.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.g2d :refer :all]))

(def screen-height 800)
(def num-tiles-per-side 50)
(def tile-size (/ screen-height num-tiles-per-side))

(def dead-map (make-array Boolean/TYPE num-tiles-per-side num-tiles-per-side))

(defn create-chicken
  "Creates a chicken entity for play-clj"
  [x y]
  (assoc (texture "ck_standing_front.png")
         :x x :y y
         :width tile-size :height tile-size
         :alive true))

(defn create-dead
  "Creates a dead body of a brave warrior for play-clj"
  [x y]
  (assoc (texture "ck_dead_back.png")
         :x x :y y
         :width tile-size :height tile-size
         :alive false))

(defn create-random-chickens
  "Create N random chickens in grid"
  [num-chickens entities]
  (into entities (repeatedly num-chickens #(create-chicken (Math/floor (rand screen-height))
                                                           (Math/floor (rand screen-height))))))

(defn create-random-dead
  "Create N random dead chickens in grid"
  [num-chickens entities]
  (into entities (repeatedly num-chickens #(create-dead (Math/floor (rand screen-height))
                                                        (Math/floor (rand screen-height))))))

(defn dec-tile [x]
  (let [new-x (- x tile-size)]
    (if (< new-x 0)
      (- screen-height tile-size)
      new-x)))

(defn inc-tile [x]
  (let [new-x (+ x tile-size)]
    (if (>= new-x screen-height)
      0
      new-x)))

(defn random-direction [] (rand-nth [:down :up :left :right]))

(defn move-chicken
  ""
  [direction entity]
  (case direction
    :down (update entity :y dec-tile)
    :up   (update entity :y inc-tile)
    :left (update entity :x dec-tile)
    :right (update entity :x inc-tile)
    nil))

(defn update-alive-chickens 
  [entities]
  (map (fn [entity]
         (->> entity
              (move-chicken (random-direction))))
       entities))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    (->> entities
         (create-random-chickens 50)))
  
  :on-render
  (fn [screen entities]
    (clear!)
    (Thread/sleep 50)
    (->> entities
         (update-alive-chickens)
         (render! screen)))

  :on-key-down
  (fn [screen entities]
    (cond
      (= (:key screen) (key-code :q))
      (app! :exit))))


(defgame hello-world-game
  :on-create
  (fn [this]
    (set-screen! this main-screen)))
