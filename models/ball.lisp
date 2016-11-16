(in-package :ball-z)


;;;
;;;
;;;
(defclass ball-mesh (simple-mesh) ()
  (:default-initargs :resource-path "models/ball.brf"))



;;;
;;;
;;;
(defclass ball-body ()
  ((r-body :initform nil)
   (geom :initform nil)))


(defmethod initialize-instance :after ((this ball-body) &key physics)
  (with-slots (r-body geom) this
    (setf r-body (make-rigid-body physics)
          geom (make-sphere-geom physics 0.5))
    (setf (mass-of r-body) (make-sphere-mass 1.0 0.5))
    (bind-geom geom r-body)))


(defun discard-body (ball-body)
  (with-slots (r-body geom) ball-body
    (dispose r-body)
    (dispose geom)))


(defmethod position-of ((this ball-body))
  (with-slots (r-body) this
    (position-of r-body)))


(defmethod (setf position-of) ((vec vec3) (this ball-body))
  (with-slots (r-body) this
    (setf (position-of r-body) vec)))


(defmethod transform-of ((this ball-body))
  (with-slots (r-body) this
    (mult (vec->translation-mat4 (position-of r-body))
          (mat->rotation-mat4 (rotation-of r-body)))))


(defun push-body (body force-vec)
  (with-slots (r-body) body
    (apply-force r-body force-vec)))


(defun body-enabled-p (body)
  (with-slots (geom) body
    (enabledp geom)))


(defun (setf body-enabled-p) (value body)
  (with-slots (geom r-body) body
    (if value
        (progn
          (enable geom)
          (enable r-body))
        (progn
          (disable geom)
          (disable r-body)))))

;;;
;;;
;;;
(defclass ball-model (model)
  ((transform :initform (identity-mat4) :accessor transform-of)
   (body :initform nil)
   (last-pos :initform (vec3))
   (last-ori :initform (vec3 0.0 0.0 -1.0))
   (simulated-p :initform nil :initarg :simulated-p :reader simulatedp)
   (sim-actions :initform '())))


(defmethod make-model-graph ((this ball-model))
  (scenegraph
   (ball-mesh)))


(defmethod simulation-pass ((this ball-model))
  (with-slots (sim-actions body) this
    (loop for fn in sim-actions
       do (funcall fn)
       finally (setf sim-actions '()))
    (when (simulatedp this)
      (setf (transform-of this) (transform-of body))))
  (call-next-method))


(defmethod rendering-pass ((this ball-model))
  (with-slots (last-pos last-ori) this
    (let ((*transform-matrix* (mult *transform-matrix* (transform-of this))))
      (if (simulatedp this)
          (call-next-method)
          (let* ((inverse-cam (inverse (transform-of *camera*)))
                 (*transform-matrix* (mult inverse-cam
                                            *transform-matrix*
                                            (vec->translation-mat4 (vec3 0.0 -1.0 -2.0)))))
            (setf last-pos (make-vec3 (mult *transform-matrix* (vec4 0.0 0.0 0.0 1.0)))
                  last-ori (normalize (mult (make-mat3 *transform-matrix*)
                                            (vec3 0.0 0.0 -1.0))))
            (call-next-method))))))


(defmethod initialize-node :after ((this ball-model) (system physics-system))
  (with-slots (body) this
    (setf body (make-instance 'ball-body :physics system))
    (unless (simulatedp this)
      (setf (body-enabled-p body) nil))))


(defmethod discard-node :before ((this ball-model))
  (with-slots (body) this
    (discard-body body)))


(defun throw-ball (ball)
  (with-slots (sim-actions body simulated-p last-pos last-ori) ball
    (push (lambda ()
            (unless (simulatedp ball)
              (setf (body-enabled-p body) t
                    (position-of body) last-pos
                    simulated-p t))
            (push-body body (mult last-ori 3000.0)))
          sim-actions)))
