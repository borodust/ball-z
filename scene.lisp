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


;;;
;;;
;;;
(defun make-main-scene ()
  (make-scene
   (scenegraph
    ((wireframe-mode :name :wireframe)
     ((projection-node :width 640 :height 480)
      ((player-camera-node :name :camera)
       (shading-pipeline-node
        ((shading-program-node
          :parameters '("modelViewProjection"
                        "normalTransform"
                        "dLight.ambient"
                        "dLight.diffuse"
                        "dLight.direction"
                        "baseColor")
          :sources (list
                    (load-shader-source :vertex-shader
                                        (resource-truename
                                         "shaders/v_ball.glsl"))
                    (load-shader-source :fragment-shader
                                        (resource-truename
                                         "shaders/f_ball.glsl"))))
         ((node :name :place))
         ((node :name :balls))
         (stage-mesh)))))
     ((start-screen-node :name :start-screen))
     (text-node)))))
