(in-package :ball-z)


;;;
;;;
;;;
(defclass ball-mesh (mesh-node)
  ((chunk :initarg :chunk)
   (transform :initform (identity-mat4))
   (bones :initform '())))


(defmethod make-node-mesh ((this ball-mesh) system)
  (with-slots (chunk transform bones) this
    (multiple-value-bind (mesh chunk-transform chunk-bones) (chunk->mesh system chunk)
      (setf transform chunk-transform
            bones chunk-bones)
      mesh)))


(defmethod rendering-pass ((this ball-mesh))
  (with-slots (transform bones) this
    (let ((*transform-matrix* (mult *transform-matrix* transform)))
      (setf (shading-parameter "normalTransform") (mat4->mat3 *transform-matrix*)
            (shading-parameter "modelViewProjection") (mult *projection-matrix*
                                                            *transform-matrix*)
            (shading-parameter "dLight.ambient") (vec3 0.2 0.2 0.2)
            (shading-parameter "dLight.diffuse") (vec3 0.8 0.8 0.8)
            (shading-parameter "dLight.direction") (vec3 -0.57735026 -0.57735026 -0.57735026))
      (call-next-method))))


(defun %load-ball-mesh ()
  (first (mesh-chunks-of (load-resource (resource-truename "models/ball.brf")))))


;;;
;;;
;;;
(defclass player-camera-node (node)
  ((circle-angle :initform 0.0)
   (pitch-angle :initform 0.0)
   (transform :initform (translation-mat4 0.0 0.0 -10.0))))


(defmethod rendering-pass ((this player-camera-node))
  (with-slots (transform) this
    (let ((*transform-matrix* (mult *transform-matrix* transform)))
      (call-next-method))))


(defun %cache-transform (cam)
  (with-slots (transform circle-angle pitch-angle) cam
    (setf transform (mult (euler-axis->mat4 #f pitch-angle (vec3 1.0 0.0 0.0))
                          (translation-mat4 0.0 0.0 -10.0)
                          (euler-axis->mat4 #f circle-angle (vec3 0.0 1.0 0.0))))))


(defun move-camera (cam angle)
  (with-slots (circle-angle) cam
    (decf circle-angle angle))
  (%cache-transform cam))


(defun pitch-camera (cam angle)
  (with-slots (pitch-angle) cam
    (let ((new-angle (max (- pi) (min pi (+ pitch-angle angle)))))
      (unless (= new-angle pitch-angle)
        (setf pitch-angle new-angle)
        (%cache-transform cam)))))

;;;
;;;
;;;
(defun make-main-scene ()
  (make-scene
   (scenegraph
    ((projection-node :width 640 :height 480)
     ((player-camera-node :name :camera)
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
        ((ball-mesh :chunk (%load-ball-mesh))))))))))
