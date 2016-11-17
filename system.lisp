(in-package :ball-z)


(defvar *main-latch* (make-latch))


(defclass ball-z (thread-bound-system)
  ((events :initform nil :reader event-system-of)
   (physics :initform nil))
  (:default-initargs :depends-on '(host-system
                                   graphics-system
                                   physics-system
                                   audio-system
                                   resource-system)))


(defmethod initialize-system :after ((this ball-z))
  (with-slots (events physics) this
    (log:config (property :log-level :info))
    (let ((host (engine-system 'host-system)))
      (setf physics (engine-system 'physics-system))
      (setf events (engine-system 'event-system))
      (register-event-classes events 'chain-broke-event)

      (when-all ((-> host
                   (setf (viewport-title host) "Ball-Z"))
                 (-> physics
                   (setf (gravity) (vec3 0.0 -9.81 0.0))))

        (subscribe-with-handler-body-to viewport-hiding-event () events
          (-> (engine)
            (shutdown)
            (log:debug "Ball-Z stopped")
            (open-latch *main-latch*)))
        (log:debug "Ball-Z started")))))


(defun strike (ctx)
  (when-let ((strike (ctx-strike ctx)))
    (let ((reg (ctx-chain-registry ctx))
          (events (ctx-event-system ctx)))
      (setf (ctx-strike ctx) nil)
      (destructuring-bind (b0 . b1) strike
        (process-strike b0 b1 reg events)))))


(defmethod make-system-context ((this ball-z))
  (with-slots (events physics) this
    (let* ((scene (make-main-scene))
           (cam (node scene :camera))
           (ctx (make-ball-z-context scene this events)))

      (subscribe-with-handler-body-to keyboard-event (ev) events
        (ge.util:with-hash-entries ((key-fn (key-from ev))) (ctx-keymap ctx)
          (let ((fn key-fn))
            (when (and fn (eq :pressed (state-from ev)))
              (funcall fn scene)))))

      (subscribe-with-handler-body-to scroll-event (ev) events
        (let* ((y (* (x-offset-from ev) 0.005))
               (x (* (y-offset-from ev) 0.005)))
          (pitch-camera cam x)
          (move-camera cam y)))

      (adopt (node scene :place)
             (make-instance 'ball-model :simulated-p nil
                            :chain-registry (ctx-chain-registry ctx)))

      (-> physics
        (register-collision-callback
         (lambda (g0 g1)
           (let ((reg (ctx-chain-registry ctx)))
             (cond
               ((process-collision reg g0 g1))
               ((when-let ((b0 (find-model-by-geom reg g0))
                           (b1 (find-model-by-geom reg g1)))
                  (cond
                    ((virginp b0)
                     (register-strike ctx b0 b1))
                    ((virginp b1)
                     (register-strike ctx b1 b0)))
                  nil))
               (t nil))))))


      (subscribe-with-handler-body-to chain-broke-event (ev) events
        (with-accessors ((balls balls-from)) ev
          (when (> (length balls) 2)
            (-> this
              (dolist (b balls)
                (abandon (parent-of b) b)
                (discard-node b))))))


      (bt:make-thread
       (lambda ()
         (let ((reg (ctx-chain-registry ctx)))
           (loop while (enabledp this) do
                (-> physics
                  (clear-links reg)
                  (observe-universe 0.014)
                  (make-chains reg)
                  (strike ctx))
                (animate scene)
                (sleep 0.014))))
       :name "scene-worker")
      ctx)))


(defun start (&optional (working-directory *default-pathname-defaults*))
  (log:config :sane2)
  (startup (setf *configuration-pathname*
                 (fad:merge-pathnames-as-file working-directory "ball-z.conf"))))


(defun main (&rest args)
  (declare (ignore args))
  (start)
  (wait-for-latch *main-latch*))
