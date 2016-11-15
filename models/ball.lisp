(in-package :ball-z)


;;;
;;;
;;;
(defclass ball-mesh (simple-mesh)
  ((r-body :initform nil)
   (geom :initform nil)
   (physics :initform nil)
   (position :initform (vec4 0.0 0.0 0.0 1.0))
   (simulated-p :initform t :initarg :simulated-p)
   (simulation-actions :initform '()))
  (:default-initargs :resource-path "models/ball.brf"))


(defmethod simulation-pass ((this ball-mesh))
  (with-slots (r-body simulated-p simulation-actions physics) this
    (when simulated-p this
          (setf (transform-of this) (mult (vec->translation-mat4 (position-of r-body))
                                          (mat->rotation-mat4 (rotation-of r-body)))))
    (loop for action in simulation-actions do
         (funcall action physics)
       finally (setf simulation-actions '())))
  (call-next-method))


(defmethod rendering-pass ((this ball-mesh))
  (with-slots (position simulated-p) this
    (unless simulated-p
      (setf position (mult *transform-matrix* (transform-of this) position))))
  (call-next-method))


(defun init-ball-body (system ball)
  (with-slots (r-body geom simulated-p position) ball
    (setf r-body (make-rigid-body system)
          geom (make-sphere-geom system 0.5)
          simulated-p t)
    (setf (position-of r-body) (vec3 (vref position 0)
                                     (vref position 1)
                                     (vref position 2)))
    (setf (mass-of r-body) (make-sphere-mass 1.0 0.5))
    (bind-geom geom r-body)))


(defmethod initialize-node :after ((this ball-mesh) (system physics-system))
  (with-slots (physics simulated-p) this
    (setf physics system)
    (when simulated-p
      (init-ball-body system this))))


(defmethod discard-node :before ((this ball-mesh))
  (with-slots (r-body geom simulated-p) this
    (when simulated-p
      (dispose r-body)
      (dispose geom))))


(defun throw-ball (ball)
  (with-slots (simulation-actions r-body simulated-p) ball
    (push (lambda (physics)
            (unless simulated-p
              (init-ball-body physics ball))
            (setf (position-of r-body) (vec3 0.0 5.0 3.0))
            (apply-force r-body (vec3 0.0 0.0 -500.0)))
          simulation-actions)))
