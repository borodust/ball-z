(in-package :ball-z)

;;;
;;;
;;;
(defclass shared-mesh (shared-resource) ())


(defmethod dispose-resource ((this shared-mesh) resource)
  (dispose resource))


;;;
;;;
;;;
(defclass ball-mesh (scene-node)
  ((path :initform "models/ball.brf" :reader resource-path-of)
   (mesh :initform nil)
   (shared-mesh :initform nil :allocation :class)
   (transform :initform (identity-mat4) :accessor transform-of :allocation :class)))


(defun load-mesh-chunk (mesh)
  (first (mesh-chunks-of (load-resource (resource-truename (resource-path-of mesh))))))


(defmethod initialize-node :after ((this ball-mesh) (system graphics-system))
  (with-slots (transform bones shared-mesh mesh) this
    (when (or (null shared-mesh) (resource-disposed-p shared-mesh))
      (multiple-value-bind (mesh chunk-transform) (chunk->mesh (load-mesh-chunk this))
        (setf transform chunk-transform)
        (setf shared-mesh (make-instance 'shared-mesh :resource mesh))))
    (setf mesh (acquire-resource shared-mesh))))


(defmethod discard-node :before ((this ball-mesh))
  (with-slots (shared-mesh) this
    (release-resource shared-mesh)))


(defmethod scene-pass ((this ball-mesh) (pass rendering-pass) input)
  (with-slots (transform mesh) this
    (let ((*transform-matrix* (mult *transform-matrix* transform)))
      (setf (shading-parameter "normalTransform") (mat4->mat3 *transform-matrix*)
            (shading-parameter "modelViewProjection") (mult *projection-matrix*
                                                            (transform-of *camera*)
                                                            *transform-matrix*)
            (shading-parameter "dLight.ambient") (vec3 0.4 0.4 0.4)
            (shading-parameter "dLight.diffuse") (vec3 0.5 0.5 0.5)
            (shading-parameter "dLight.direction") (vec3 0.0 -1.0 0.0))
      (render mesh)))
  (call-next-method))
