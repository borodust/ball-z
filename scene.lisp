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


(defclass transform-to-origin-node (node) ())


(defmethod rendering-pass ((this transform-to-origin-node))
  (let ((*transform-matrix* (mult *transform-matrix* (inverse *transform-matrix*))))
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
         (transform-to-origin-node
          ((transform-node :translation (vec3 0.0 -1.0 -2.0) :name :place)
           ((ball-mesh :simulated-p nil :name :ball))))
         ((node :name :balls)
          (ball-mesh)
          (ball-mesh)
          (ball-mesh)
          (stage-mesh))))))))))
