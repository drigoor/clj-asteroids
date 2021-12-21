(ns clj-asteroids.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))


;; tic80 games to replicate
;;
;;
;; BALL UP
;;    https://tic80.com/play?cart=2355



;; https://ostash.dev/posts/
;;    e.g.: Data notation in Clojure
;; (let [uuid #uuid "f81d4fae-7dec-11d0-a765-00a0c91e6bf6"]
;; 	(class uuid))


;; ;; from: http://makble.com/how-to-generate-uuid-in-clujure
;; (defn show [ t ]
;;   (map #(println %) (seq (:declaredMethods (bean t)))))
;; ;; (show java.util.UUID)


(defn create-entity []
  (java.util.UUID/randomUUID))



(component renderable [func]
           :fn func)


(defn renderer [renderables]
  (doseq [e renderables]
    (let [rend (as e :renderable)]
      ((rend :fn) e))))


(defmacro component [name params & body]
  `(defn ~name ~params
     {~(keyword name) ~(apply hash-map body)}))


(component position [x y]
           :x x
           :y y)


(component health [lives]
           :lives lives
           :health 100)


(defmacro entity [name & body]
  `(merge {:name ~(keyword name)} ~@body))


;; name is a keyword
(defn entity [name & body]
  (apply merge {:name name} body))


(entity :player
        (position 100 100)
        (health 3))


(let [player (entity :player
                     (position 100 100)
                     (health 3))
      {x :x y :y} (:position player)]
  (println "player position: " x ))


;; ---------------------------------------------------------------------




;; based on: http://www.quil.info/sketches/show/example_nanoscopic


(defn create-game-state []
  {:x 0
   :y 0
   :x-speed 1
   :y-speed 3.3})


(defn setup-state []
  (q/frame-rate 60)
  (q/color-mode :rgb)
  (q/smooth)
  (create-game-state))


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
  (q/background 246 245 236)
                                        ; black
  (q/fill (q/color 246 245 236))
  (q/rect 1 1 100 100)
                                        ; semitransparent gray
  (q/fill (q/color 128 128))
  (q/rect 70 70 100 100)
                                        ; purple
  (q/fill (q/color 255 0 255))
  (q/rect 140 140 100 100)
                                        ; semitransparent cyan
  (q/fill (q/color 0 255 255 120))
  (q/rect 210 210 100 100)
  
  ;; (q/fill (q/color 246 245 236))
  ;; (q/ellipse (:x state) (:y state) 48 48)
  ;; (q/stroke 127)
  ;; (q/stroke-weight 2)
  ;; (q/ellipse (:x state) (:y state) 48 48)
  ;; (q/fill 0)
  ;; ;; from: http://quil.info/api/input/keyboard
  ;; (doseq [[ind capt fn] [[0 "key-as-keyword" q/key-as-keyword]
  ;;                        [1 "key-code" q/key-code]
  ;;                        [2 "key-coded?" (fn* [] (q/key-coded? (q/raw-key)))]
  ;;                        [3 "key-pressed?" q/key-pressed?]
  ;;                        [4 "raw-key" q/raw-key]
  ;;                        [5 "key-modifiers" q/key-modifiers]]]
  ;;   (q/text (str capt " " (fn)) 10 (+ (* 20 ind) 20)))
  )


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
  :middleware [m/fun-mode])
