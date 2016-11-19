(in-package :ball-z)

;;;
;;;
;;;
(defclass shared-mesh (shared-resource) ())


(defmethod dispose-resource :before ((this shared-mesh) resource)
  (dispose resource))


;;;
;;;
;;;
(defclass ball-mesh (mesh-node)
  ((path :initform "models/ball.brf" :reader resource-path-of)
   (shared-mesh :initform nil :allocation :class)
   (transform :initform (identity-mat4) :accessor transform-of :allocation :class)))


(defun load-mesh-chunk (mesh)
  (first (mesh-chunks-of (load-resource (resource-truename (resource-path-of mesh))))))


(defmethod make-node-mesh ((this ball-mesh) system)
  (with-slots (transform bones shared-mesh) this
    (when (or (null shared-mesh) (resource-disposed-p shared-mesh))
      (multiple-value-bind (mesh chunk-transform) (chunk->mesh system (load-mesh-chunk this))
        (setf transform chunk-transform)
        (setf shared-mesh (make-instance 'shared-mesh :resource mesh))))
    (acquire-resource shared-mesh)))


(defmethod discard-node :before ((this ball-mesh))
  (with-slots (shared-mesh) this
    (release-resource shared-mesh)))


(defmethod rendering-pass ((this ball-mesh))
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