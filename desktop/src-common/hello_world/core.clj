(ns hello-world.core
  (:require [hello-world.chicken-system :as cs]
            [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.g2d :refer :all]
            clojure.pprint))

(def screen-height 800)
(def tile-size (/ screen-height cs/dimension))

(defn create-chicken-entity
  "Creates a chicken entity for play-clj"
  [x y]
  (assoc (texture "ck_standing_front.png")
         :x x :y y
         :width tile-size :height tile-size
         :alive true))

(defn create-body-entity
  "Creates a dead body of a brave warrior for play-clj"
  [x y]
  (assoc (texture "ck_dead_back.png")
         :x x :y y
         :width tile-size :height tile-size
         :alive false))

(defn create-chicken-entities
  [ants entities-input]
  (loop [entities entities-input
         ant-ref (first ants)
         left-ants (rest ants)] 
    (if (nil? ant-ref)
      entities
      (let [ant (deref ant-ref)] 
        (recur (conj entities (create-chicken-entity (* tile-size (:x ant)) (* tile-size (:y ant))))
               (first left-ants)
               (rest left-ants))))))

(defn update-chicken-entities-positions
  ""
  [entities]
  (loop [entity        (first entities)
         left-entities (rest entities)
         ant-ref   (first cs/ants)
         left-ants (rest  cs/ants)
         out-entities []]
    (if (nil? ant-ref)
      (concat out-entities [entity] left-entities)
      (let [ant (deref ant-ref)]
        (recur (first left-entities) (rest left-entities)
               (first left-ants)     (rest left-ants)
               (conj out-entities (assoc (assoc entity :y (* tile-size (:y ant)))
                                         :x (* tile-size (:x ant)))))))))

(defn create-bodies-entities
  [bodies entities-input]
  (loop [entities entities-input         
         body-ref (first bodies)
         left-bodies (rest bodies)]
    (if (nil? body-ref)
      entities
      (let [body (deref body-ref)]
        (recur (conj entities (create-body-entity (* tile-size (:x body)) (* tile-size (:y body))))
               (first left-bodies)
               (rest left-bodies))))))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    (clojure.pprint/pprint cs/ants)
    (->> entities
         (create-chicken-entities cs/ants)
         (create-bodies-entities cs/bodies)))
  
  :on-render
  (fn [screen entities]
    (clear!)
    (Thread/sleep 50)
    (cs/loop-ants)
    (->> entities
         (update-chicken-entities-positions)
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

