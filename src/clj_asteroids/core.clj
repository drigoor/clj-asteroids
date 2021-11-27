(ns clj-asteroids.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))


(defn setup-state []
  (q/frame-rate 60)
  (q/color-mode :hsb)
  (q/smooth)
  {:x 0
   :y 0
   :x-speed 1
   :y-speed 3.3})


(defn update-state [state]
  (let [x (if (and (:key-a state) (:key-d state))
            (:x state)
            (if (:key-d state)
              (+ (:x state) (:x-speed state))
              (if (:key-a state)
                (- (:x state) (:x-speed state))
                (:x state))))
        y (+ (:y state) (:y-speed state))
        x-speed (if (or (> x (q/width))
                        (< x 0))
                  (* -1 (:x-speed state))
                  (:x-speed state))
        y-speed (if (or (> y (q/height))
                        (< y 0))
                  (* -1 (:y-speed state))
                  (:y-speed state))]
    (assoc state :x x :y y :x-speed x-speed :y-speed y-speed)))


(defn draw-state [state]
  (q/background 240)
  (q/fill 255)
  (q/stroke 127)
  (q/stroke-weight 2)
  (q/ellipse (:x state) (:y state) 48 48)
  (q/fill 0)
  ;; from: http://quil.info/api/input/keyboard
  (doseq [[ind capt fn] [[0 "key-as-keyword" q/key-as-keyword]
                         [1 "key-code" q/key-code]
                         [2 "key-coded?" (fn* [] (q/key-coded? (q/raw-key)))]
                         [3 "key-pressed?" q/key-pressed?]
                         [4 "raw-key" q/raw-key]
                         [5 "key-modifiers" q/key-modifiers]]]
    (q/text (str capt " " (fn)) 10 (+ (* 20 ind) 20))))


(defn key-pressed [state event]
  (let [new-state (if (= (:key event) :a)
                    (assoc state :key-a true)
                    
                    ;; https://stackoverflow.com/questions/14824786/using-wasd-and-arrow-keys-simultaneously
                    
                    state)]
    (if (= (:key event) :d)
      (assoc new-state :key-d true)
      new-state)))


(defn key-released [state event]
  (let [new-state (if (= (:key event) :a)
                    (assoc state :key-a false)
                    
                    ;; https://stackoverflow.com/questions/14824786/using-wasd-and-arrow-keys-simultaneously
                    
                    state)]
    (if (= (:key event) :d)
      (assoc new-state :key-d false)
      new-state)))


(q/defsketch clj-asteroids
  :title "clj-asteroids"
  :size [500 500]
  :setup setup-state
  :update update-state
  :draw draw-state
  :key-pressed key-pressed
  :key-released key-released
  :features [:keep-on-top]
  :middleware [m/fun-mode])
