(in-package :ball-z)

;;;
;;;
;;;
;;;
(defvar *colors* (list (cons :red (vec4 1.0 0.0 0.0 1.0))
                       (cons :green (vec4 0.0 1.0 0.0 1.0))
                       (cons :blue (vec4 0.0 0.0 1.0 1.0))
                       (cons :white (vec4 1.0 1.0 1.0 1.0))
                       (cons :yellow (vec4 1.0 1.0 0.0 1.0))))

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


(defmethod model-graph-assembly-flow ((this ball-model))
  (scenegraph
   (ball-mesh)))


(defun (setf ball-position) (value ball)
  (with-slots (body) ball
    (setf (position-of body) value)))


(defun ball-assembly-flow (reg &optional pos)
  (assembly-flow 'ball-model :simulated-p t :virgin-p nil :chain-registry reg
                 :position (or pos (vec3))))


(defmethod initialization-flow ((this ball-model) &key)
  (with-slots (body chain-registry type last-pos sounds audio) this
    (~> (call-next-method)
        (-> ((physics)) ()
          (setf body (make-instance 'ball-body
                                    :registry chain-registry
                                    :model this
                                    :position last-pos))
          (unless (simulatedp this)
            (setf (body-enabled-p body) nil)))
        (-> ((audio)) ()
          (setf audio (audio)
                sounds (make-instance 'ball-audio))))))


(defmethod scene-pass ((this ball-model) (pass simulation-pass) input)
  (with-slots (sim-actions body linked-p) this
    (loop for fn in sim-actions
       do (funcall fn)
       finally (setf sim-actions '()))
    (when (simulatedp this)
      (setf (transform-of this) (transform-of body)
            linked-p (linked-body-p body))))
  (call-next-method this pass input))


(defmethod scene-pass ((this ball-model) (pass rendering-pass) input)
  (with-slots (last-pos last-ori type linked-p) this
    (setf (shading-parameter "baseColor") (mult (cdr (assoc type *colors*))
                                                (if linked-p 5.0 1.0)))
    (let ((*model-matrix* (mult *model-matrix* (transform-of this))))
      (if (simulatedp this)
          (call-next-method)
          (let* ((inverse-cam (inverse (transform-of *camera*)))
                 (*model-matrix* (mult inverse-cam
                                           *model-matrix*
                                           (vec->translation-mat4 (vec3 0.0 -1.0 -2.5)))))
            (setf last-pos (make-vec3 (mult *model-matrix* (vec4 0.0 0.0 0.0 1.0)))
                  last-ori (normalize (mult (make-mat3 *model-matrix*)
                                            (vec3 0.0 0.0 -1.0))))
            (call-next-method))))))


(defmethod discard-node :before ((this ball-model))
  (with-slots (body sounds) this
    (discard-body body)
    (dispose sounds)))


(defmacro when-simulating ((ball) &body body)
  (with-gensyms (a)
    `(with-slots ((,a sim-actions)) ,ball
       (push (lambda () ,@body) ,a))))


(defun push-ball (ball f-vec)
  (with-slots (body simulated-p) ball
    (when-simulating (ball)
      (unless (simulatedp ball)
        (setf (body-enabled-p body) t
              simulated-p t))
      (push-body body f-vec))))


(defun throw-ball (ball)
  (with-slots (sim-actions body simulated-p last-pos last-ori audio sounds) ball
    (when-simulating (ball)
      (setf (position-of body) last-pos))
    (run (-> (audio) ()
           (play-pop-sound sounds)))
    (push-ball ball (mult last-ori 3000.0))))


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
            (run (-> (audio) ()
                   (play-strike-sound sounds)))))
        (run (-> (audio) ()
               (play-fail-sound sounds))))))
