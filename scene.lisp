(in-package :ball-z)


;;;
;;;
;;;
(defun make-main-scenegraph (ctx)
  (scenegraph
   ((transform-node)
    ((projection-node :aspect (/ 640 480))
     ((player-camera-node :name :camera)
      (shading-pipeline-node
       ((shading-program-node
         :sources (list
                   (load-shader-source :vertex-shader
                                       (resource-truename
                                        "shaders/v_ball.glsl"))
                   (load-shader-source :fragment-shader
                                       (resource-truename
                                        "shaders/f_ball.glsl"))))
        ((scene-node :name :place))
        ((scene-node :name :balls))
        (stage-mesh)))))
    ((hud-node :name :hud))
    ((start-screen-node :name :start-screen))
    ((debug-node :name :debug :metrics (ctx-metrics ctx))))))
