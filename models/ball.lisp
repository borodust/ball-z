(in-package :ball-z)


;;;
;;;
;;;
(defclass ball-mesh (simple-mesh)
  ((r-body :initform nil)
   (geom :initform nil))
  (:default-initargs :resource-path "models/ball.brf"))


(defmethod simulation-pass ((this ball-mesh))
  (with-slots (r-body) this
    (setf (transform-of this) (mult (vec->translation-mat4 (position-of r-body))
                                    (mat->rotation-mat4 (rotation-of r-body)))))
  (call-next-method))


(defmethod initialize-node :after ((this ball-mesh) (system physics-system))
  (with-slots (r-body geom) this
    (setf r-body (make-rigid-body system)
          geom (make-sphere-geom system 0.5))
    (bind-geom geom r-body)))


(defmethod discard-node :before ((this ball-mesh))
  (with-slots (r-body geom) this
    (dispose r-body)
    (dispose geom)))
