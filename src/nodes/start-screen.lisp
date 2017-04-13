(in-package :ball-z)


(defclass start-screen-node (enableable-node scene-node)
  ((anatolian-renderer :initform nil)
   (canvas :initform nil)
   (image-paint :initform nil)))


(defmethod initialization-flow ((this start-screen-node) &key)
  (with-slots (anatolian-renderer canvas image-paint) this
    (>> (call-next-method)
        (resource-flow (font-resource-name "Anatolian.ttf"))
        (-> ((graphics)) (anatolian-font)
          ;; fixme: load thru assets
          (let ((banner (load-png-image (resource-truename "images/ball-z.png"))))
            (setf anatolian-renderer (make-text-renderer 640 480 anatolian-font 24)
                  canvas (make-canvas 640 480 :antialiased t)
                  image-paint (make-image-paint banner
                                                :origin (vec2 (- 320 (/ 439 2)) 240)
                                                :flip-vertically t
                                                :canvas canvas)))))))


(defmethod scene-pass ((this start-screen-node) (pass rendering-pass) input)
  (with-slots (anatolian-renderer image-paint canvas) this
    (draw-text anatolian-renderer "PRESS ENTER TO START" :position (vec2 (- 320 124) 240))

    (with-canvas (canvas)
      (draw-rect (vec2 (- 320 (/ 439 2)) 240) 439 105 :fill-paint image-paint)))
  (call-next-method))
