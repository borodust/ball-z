(in-package :ball-z)

;;;
;;;
(defclass ball-body ()
  ((r-body :initform nil)
   (geom :initform nil :reader geom-of)
   (bounds :initform nil :reader bounds-of)
   (registry :initarg :registry)))


(defmethod initialize-instance :after ((this ball-body) &key physics model)
  (with-slots (r-body geom bounds registry) this
    (setf r-body (make-rigid-body physics)
          geom (make-sphere-geom physics 0.5)
          bounds (make-sphere-geom physics 0.575))
    (register-bounding-geom registry bounds model)
    (register-model-geom registry model geom)
    (setf (mass-of r-body) (make-sphere-mass 1.0 0.5))
    (bind-geom geom r-body)
    (bind-geom bounds r-body)))


(defun discard-body (ball-body)
  (with-slots (r-body geom bounds) ball-body
    (dispose r-body)
    (dispose geom)
    (dispose bounds)))


(defun linked-body-p (body)
  (with-slots (registry bounds) body
    (linked-p registry bounds)))


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
  (with-slots (geom r-body bounds) body
    (if value
        (progn
          (enable geom)
          (enable bounds)
          (enable r-body))
        (progn
          (disable geom)
          (disable bounds)
          (disable r-body)))))
