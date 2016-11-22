(in-package :ball-z)

;;;
;;;
;;;
;;;
(defvar *colors* (list (cons :red (vec3 1.0 0.0 0.0))
                       (cons :green (vec3 0.0 1.0 0.0))
                       (cons :blue (vec3 0.0 0.0 1.0))
                       (cons :white (vec3 1.0 1.0 1.0))
                       (cons :yellow (vec3 1.0 1.0 0.0))))

(defvar *types* #(:red :green :blue :white :yellow))

(defclass ball-model (model)
  ((transform :initform (identity-mat4) :accessor transform-of)
   (body :initform nil)
   (type :initform (aref *types* (random (length *types*))) :reader ball-type-of)
   (linked-p :initform nil)
   (last-pos :initform (vec3) :initarg :position)
   (last-ori :initform (vec3 0.0 0.0 -1.0))
   (virgin-p :initform t :initarg :virgin-p :reader virginp)
   (simulated-p :initform nil :initarg :simulated-p :reader simulatedp)
   (sim-actions :initform '())
   (chain-registry :initarg :chain-registry)
   (sounds :initform nil)
   (audio :initform nil)))


(defmethod make-model-graph ((this ball-model))
  (scenegraph
   (ball-mesh)))


(defun (setf ball-position) (value ball)
  (with-slots (body) ball
    (setf (position-of body) value)))


(defun make-ball (reg &optional pos)
  (make-instance 'ball-model :simulated-p t :virgin-p nil :chain-registry reg
                 :position (or pos (vec3))))


(defmethod simulation-pass ((this ball-model))
  (with-slots (sim-actions body linked-p) this
    (loop for fn in sim-actions
       do (funcall fn)
       finally (setf sim-actions '()))
    (when (simulatedp this)
      (setf (transform-of this) (transform-of body)
            linked-p (linked-body-p body))))
  (call-next-method))


(defmethod rendering-pass ((this ball-model))
  (with-slots (last-pos last-ori type linked-p) this
    (setf (shading-parameter "baseColor") (mult (cdr (assoc type *colors*))
                                                (if linked-p 5.0 1.0)))
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
  (with-slots (body chain-registry type last-pos) this
    (setf body (make-instance 'ball-body
                              :physics system
                              :registry chain-registry
                              :model this
                              :position last-pos))
    (unless (simulatedp this)
      (setf (body-enabled-p body) nil))))


(defmethod initialize-node :after ((this ball-model) (system audio-system))
  (with-slots (sounds audio) this
    (setf audio system
          sounds (make-instance 'ball-audio :audio system))))


(defmethod discard-node :before ((this ball-model))
  (with-slots (body sounds) this
    (discard-body body)
    (dispose sounds)))


(defmacro when-simulating ((ball) &body body)
  (with-gensyms (a)
    `(with-slots ((,a sim-actions)) ,ball
       (push (lambda () ,@body) ,a))))


(defun throw-ball (ball)
  (with-slots (sim-actions body simulated-p last-pos last-ori audio sounds) ball
    (when-simulating (ball)
      (setf (position-of body) last-pos))
    (-> (audio)
      (play-pop-sound sounds))
    (push-ball ball (mult last-ori 3000.0))))


(defun push-ball (ball f-vec)
  (with-slots (body simulated-p) ball
    (when-simulating (ball)
      (unless (simulatedp ball)
        (setf (body-enabled-p body) t
              simulated-p t))
      (push-body body f-vec))))


(defun loose-virginity (ball)
  (with-slots (virgin-p) ball
    (setf virgin-p nil)))


(defun process-strike (virgin other reg events)
  (loose-virginity virgin)
    (with-slots (body audio sounds) other
      (if (eq (ball-type-of virgin)
              (ball-type-of other))
          (when-let ((balls (find-model-chain-by-bounding-geom reg (bounds-of body))))
            (pushnew virgin balls)
            (when (> (length balls) 2)
              (post (make-chain-broke-event balls) events)
              (-> (audio)
                (play-strike-sound sounds))))
          (-> (audio)
            (play-fail-sound sounds)))))
