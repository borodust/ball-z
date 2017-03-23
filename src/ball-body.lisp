(in-package :ball-z)

;;;
;;;
;;;
(defclass ball-geom (sphere-geom) ())


(defmethod collide ((this ball-geom) that)
  nil)


(defmethod collide (this (that ball-geom))
  (collide that this))


(defmethod filter-contacts (contacts (this ball-geom) (that ball-geom))
  (when contacts
    (run (-> ((ball-z)) ()
           (let ((reg (ctx-chain-registry *system-context*)))
             (when-let ((b0 (find-model-by-geom reg this))
                        (b1 (find-model-by-geom reg that)))
               (cond
                 ((virginp b0)
                  (register-strike *system-context* b0 b1))
                 ((virginp b1)
                  (register-strike *system-context* b1 b0))))))))
  contacts)

;;;
;;;
;;;
(defclass ball-bounds (sphere-geom) ())


(defmethod collide ((this ball-bounds) (that ball-bounds))
  nil)

(defmethod filter-contacts (contacts (this ball-bounds) (that ball-bounds))
  (when contacts
    (run (-> ((ball-z)) ()
           (let ((reg (ctx-chain-registry *system-context*)))
             (process-bounds-collision reg this that)))))
  nil)


;;;
;;;
(defclass ball-body ()
  ((r-body :initform nil)
   (geom :initform nil :reader geom-of)
   (bounds :initform nil :reader bounds-of)
   (registry :initarg :registry)))


(defmethod initialize-instance :after ((this ball-body) &key model position)
  (with-slots (r-body geom bounds registry) this
    (setf r-body (make-rigid-body)
          geom (make-instance 'ball-geom :radius 0.5)
          bounds (make-instance 'ball-bounds :radius 0.575))
    (register-bounding-geom registry bounds model)
    (register-model-geom registry model geom)
    (setf (mass-of r-body) (make-sphere-mass 1.0 0.5))
    (setf (position-of r-body) position)
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
