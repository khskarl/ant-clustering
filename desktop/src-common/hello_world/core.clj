(ns hello-world.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.g2d :refer :all]))

(def screen-height 800)
(def tile-size 25)
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

;; (defn update-alive-chickens 
;;   [entities]
;;   (map (fn [entity]
;;          (->> entity
;;               (move-chicken (random-direction))))
;;        entities))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    ;; (->> entities
    ;;      (create-random-chickens 50))
    )
  
  
  :on-render
  (fn [screen entities]
    (clear!)
    (Thread/sleep 50)
    (->> entities
         ;; (update-alive-chickens)
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

