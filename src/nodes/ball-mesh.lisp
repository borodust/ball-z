(in-package :ball-z)

;;;
;;;
;;;
(defclass ball-mesh (scene-node)
  ((mesh :initform nil)
   (transform :initform (identity-mat4) :accessor transform-of :allocation :class)))


(defmethod initialization-flow ((this ball-mesh) &key)
  (with-slots (transform (m mesh)) this
    (>> (call-next-method)
        (asset-flow "Sphere.0")
        (-> ((graphics)) (mesh)
          (setf m (mesh-asset-mesh mesh)
                transform (mesh-asset-transform mesh))))))


(defmethod scene-pass ((this ball-mesh) (pass rendering-pass) input)
  (with-slots (transform mesh) this
    (let ((*model-matrix* (mult *model-matrix* transform)))
      (setf (shading-parameter "normalTransform") (mat4->mat3 *model-matrix*)
            (shading-parameter "modelViewProjection") (mult *projection-matrix*
                                                            (transform-of *camera*)
                                                            *model-matrix*)
            (shading-parameter "dLight.ambient") (vec4 0.4 0.4 0.4 1.0)
            (shading-parameter "dLight.diffuse") (vec4 0.5 0.5 0.5 1.0)
            (shading-parameter "dLight.direction") (vec3 0.0 -1.0 0.0))
      (render mesh)))
  (call-next-method))
