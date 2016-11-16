(in-package :ball-z)



;;;
;;;
;;;
(defclass wireframe-mode (node)
  ((enabled-p :initform nil :initarg :enabled-p :accessor enabledp)))


(defmethod rendering-pass ((this wireframe-mode))
  (if (enabledp this)
      (in-wireframe-mode
        (call-next-method))
      (call-next-method)))


(defclass camera-space-node (node) ())


(defmethod rendering-pass ((this camera-space-node))
  (let ((*camera-transform* (mult *camera-transform* (inverse *camera-transform*))))
    (call-next-method)))

;;;
;;;
;;;
(defun make-main-scene ()
  (make-scene
   (scenegraph
    ((projection-node :width 640 :height 480)
     ((player-camera-node :name :camera)
      ((wireframe-mode :name :wireframe)
       (shading-pipeline-node
        ((shading-program-node
          :parameters '("modelViewProjection"
                        "normalTransform"
                        "dLight.ambient"
                        "dLight.diffuse"
                        "dLight.direction")
          :sources (list
                    (load-shader-source :vertex-shader
                                        (resource-truename
                                         "shaders/v_ball.glsl"))
                    (load-shader-source :fragment-shader
                                        (resource-truename
                                         "shaders/f_ball.glsl"))))
         (camera-space-node
          ((transform-node :translation (vec3 0.0 -1.0 -2.0) :name :place)
           ((ball-model :simulated-p nil))))
         ((node :name :balls)
          ((ball-model :simulated-p t)))
         (stage-mesh)))))))))
