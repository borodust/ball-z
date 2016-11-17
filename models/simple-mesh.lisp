(in-package :ball-z)


;;;
;;;
;;;
(defclass simple-mesh (mesh-node)
  ((path :initarg :resource-path :reader resource-path-of)
   (transform :initform (identity-mat4) :accessor transform-of :allocation :class)))


(defun load-mesh-chunk (mesh)
  (first (mesh-chunks-of (load-resource (resource-truename (resource-path-of mesh))))))


(defmethod make-node-mesh ((this simple-mesh) system)
  (with-slots (transform bones shared-mesh) this
    (multiple-value-bind (mesh chunk-transform) (chunk->mesh system (load-mesh-chunk this))
      (setf transform chunk-transform)
      mesh)))


(defmethod rendering-pass ((this simple-mesh))
  (with-slots (transform) this
    (let ((*transform-matrix* (mult *transform-matrix* transform)))
      (setf (shading-parameter "normalTransform") (mat4->mat3 *transform-matrix*)
            (shading-parameter "modelViewProjection") (mult *projection-matrix*
                                                            (transform-of *camera*)
                                                            *transform-matrix*)
            (shading-parameter "dLight.ambient") (vec3 0.2 0.2 0.2)
            (shading-parameter "dLight.diffuse") (vec3 0.8 0.8 0.8)
            (shading-parameter "dLight.direction") (vec3 -0.57735026 -0.57735026 -0.57735026))
      (call-next-method))))
