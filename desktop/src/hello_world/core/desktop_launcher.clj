(ns hello-world.core.desktop-launcher
  (:require [hello-world.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. hello-world-game "Galinhas" 800 800)
  (Keyboard/enableRepeatEvents true))
