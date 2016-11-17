(in-package :ball-z)


(defvar *main-latch* (make-latch))


(defclass ball-z (thread-bound-system)
  ((events :initform nil)
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


(defmethod make-system-context ((this ball-z))
  (with-slots (events physics) this
    (let* ((scene (make-main-scene))
           (cam (node scene :camera))
           (ctx (make-ball-z-context scene this)))

      (subscribe-with-handler-body-to keyboard-event (ev) events
        (ge.util:with-hash-entries ((key-fn (key-from ev))) (ctx-keymap ctx)
          (let ((fn key-fn))
            (when (and fn (eq :pressed (state-from ev)))
              (-> this
                (funcall fn scene))))))

      (subscribe-with-handler-body-to scroll-event (ev) events
        (let* ((y (* (x-offset-from ev) 0.005))
               (x (* (y-offset-from ev) 0.005)))
          (-> this
            (pitch-camera cam x)
            (move-camera cam y))))

      (adopt (node scene :place)
             (make-instance 'ball-model :simulated-p nil
                            :chain-registry (ctx-chain-registry ctx)))

      (-> physics
        (register-collision-callback
         (lambda (g0 g1)
           (process-collision (ctx-chain-registry ctx) g0 g1))))

      (bt:make-thread
       (lambda ()
         (let ((reg (ctx-chain-registry ctx)))
           (loop while (enabledp this) do
                (when-all* ((clear-links reg)
                            (-> physics
                              (observe-universe 0.014))
                            (make-chains reg)
                            (animate scene)))
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
