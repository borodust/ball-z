(in-package :ball-z)


(defvar *main-latch* (make-latch))


(defclass ball-z (thread-bound-system)
  ()
  (:default-initargs :depends-on '(host-system
                                   graphics-system
                                   physics-system
                                   audio-system
                                   asset-system)))


(definline ball-z ()
  (engine-system 'ball-z))


(defun exit-game ()
  (in-new-thread-waiting "exit-worker"
    (shutdown)
    (log:debug "Ball-Z stopped")
    (open-latch *main-latch*)))


(defmethod initialize-system :after ((this ball-z))
  (log:config (property :log-level :info))
  (register-event-classes (events)
                          'chain-broke-event
                          'game-loaded-event
                          'game-started-event
                          'game-ended-event)
  (register-asset-loader (asset-registry-of (assets))
                         (make-resource-loader (resource-truename "models/ball.brf")
                                               (resource-truename "fonts/fonts.brf")))

    (run (>> (-> ((host)) ()
               (setf (viewport-title) "Ball-Z")
               (lock-cursor))
             (-> ((physics)) ()
               (setf (gravity) (vec3 0.0 -9.81 0.0)))))

    (subscribe-body-to (viewport-hiding-event ()) (events)
      (exit-game)))


(defun strike (ctx)
  (when-let ((strike (pop-strike ctx)))
    (let ((reg (ctx-chain-registry ctx)))
      (destructuring-bind (b0 . b1) strike
        (process-strike b0 b1 reg (events))))))


(defun throw-ball-action (registry)
  (lambda (s)
    (let* ((place (node s :place))
           (balls (node s :balls))
           (ball (first (children-of place))))
      (run (>> (assembly-flow 'ball-model
                              :simulated-p nil
                              :chain-registry registry)
            (-> ((ball-z) :priority :high) (new-ball)
             (abandon place ball)
             (adopt balls ball)
             (throw-ball ball)
             (adopt place new-ball)))))))


(defun start-game-action (sys reg)
  (lambda (s)
    (run (-> (sys :priority :high) ()
           (disable-node (node s :start-screen))
           (bind-key :space (throw-ball-action reg))
           (post (make-instance 'game-started-event) (events))))))


(defun generate-balls (ctx)
  (let* ((s (ctx-scene ctx))
         (registry (ctx-chain-registry ctx))
         (balls (node s :balls)))
    (in-new-thread "ball-gen"
      (dotimes (i 192)
        (let* ((angle (* (/ pi 32) (mod i 64)))
               (x (cos angle))
               (z (sin angle))
               (direction (vec3 x 0.0 z)))
          (run (>> (ball-assembly-flow registry (mult direction 1.0))
                   (instantly (b)
                     (adopt balls b)
                     (push-ball b (mult direction 1500)))))
          (sleep 0.05))))))


(defun remove-balls (ctx)
  (let* ((s (ctx-scene ctx))
         (balls (node s :balls)))
    (loop for ball in (children-of balls)
       do (abandon balls ball)
         (discard-node ball))))


(defun update-hud (ctx)
  (let* ((hud-node (node (ctx-scene ctx) :hud))
         (started-at (ctx-timer-started ctx))
         (now (floor (epoch-seconds)))
         (max (* 3 60))
         (passed (- now (floor (or started-at 0))))
         (left (max 0 (- max passed)))
         (min (if started-at (floor (/ left 60)) 3))
         (sec (if started-at (mod left 60) 0)))

    (update-score-text hud-node (ctx-score ctx))

    (when (and started-at (> passed max))
      (reset-timer ctx)
      (post (make-instance 'game-ended-event) (events)))
    (update-timer-text hud-node min sec)))


(defun make-default-keymap ()
  (ge.util:make-hash-table-with-entries () ((w :w) (a :a) (s :s) (d :d)
                                            (esc :escape))
    (setf w (lambda (s)
              (let ((cam (node s :camera)))
                (pitch-camera cam -0.1)))
          a (lambda (s)
              (let ((cam (node s :camera)))
                (move-camera cam -0.1)))
          s (lambda (s)
              (let ((cam (node s :camera)))
                (pitch-camera cam 0.1)))
          d (lambda (s)
              (let ((cam (node s :camera)))
                (move-camera cam 0.1)))
          esc (lambda (s)
                (declare (ignore s))
                (exit-game)))))


(defun init-context (this ctx)
  (let* ((events (events))
         (scene (ctx-scene ctx))
         (cam (node scene :camera))
         (reg (ctx-chain-registry ctx)))

    (subscribe-body-to (keyboard-event (key state)) events
      (ge.util:with-hash-entries ((key-fn key)) (ctx-keymap ctx)
        (let ((fn key-fn))
          (when (and fn (eq :pressed state))
            (funcall fn scene)))))

    (subscribe-body-to (scroll-event () ev) events
      (let* ((y (* (x-offset-from ev) 0.005))
             (x (- (* (y-offset-from ev) 0.005))))
        (pitch-camera cam x)
        (move-camera cam y)))

    (let ((prev-x nil)
          (prev-y nil))
      (subscribe-body-to (cursor-event (x y)) events
        (when (and prev-x prev-y)
          (let* ((cur-y (* (- x prev-x) 0.006))
                 (cur-x (* (- prev-y y) 0.0025)))
            (pitch-camera cam cur-x)
            (move-camera cam cur-y)))
        (setf prev-x x
              prev-y y)))

    (subscribe-body-to (mouse-event (state button)) events
      (when (and (eq :pressed state)
                 (eq :left button))
        (execute-key-action ctx :space)))

    (subscribe-body-to (game-loaded-event ()) events
      (run (-> (this :priority :high) ()
             (bind-key :enter (start-game-action this reg)))))

    (run (>> (assembly-flow 'ball-model :simulated-p nil
                            :chain-registry (ctx-chain-registry ctx))
             (-> (this) (node)
               (adopt (node scene :place) node))))

    ;; (-> (physics)
    ;;   (register-collision-callback
    ;;    (lambda (g0 g1)
    ;;      (process-bounds-collision reg g0 g1)))

    ;;   (register-contact-callback
    ;;    (lambda (g0 g1)
    ;;      (let ((reg (ctx-chain-registry ctx)))
    ;;        (when-let ((b0 (find-model-by-geom reg g0))
    ;;                   (b1 (find-model-by-geom reg g1)))
    ;;          (cond
    ;;            ((virginp b0)
    ;;             (register-strike ctx b0 b1))
    ;;            ((virginp b1)
    ;;             (register-strike ctx b1 b0)))
    ;;          nil))))))

    (subscribe-body-to (chain-broke-event (balls)) events
      (run (-> (this) ()
             (update-score ctx (* (length balls) 10))
             (dolist (b balls)
               (abandon (parent-of b) b)
               (discard-node b)))))

    (subscribe-body-to (game-started-event ()) events
      (run (-> (this :priority :high) ()
             (bind-key :enter (lambda (s) (declare (ignore s))))
             (remove-balls ctx)
             (reset-score ctx)
             (start-timer ctx)
             (generate-balls ctx))))

    (subscribe-body-to (game-ended-event ()) events
      (run (-> (this :priority :high) ()
             (reset-timer ctx)
             (bind-key :enter (start-game-action this reg))
             (bind-key :space (lambda (s) (declare (ignore s))))
             (enable-node (node scene :start-screen)))))

    (in-new-thread "music-loader"
      (load-background-music ctx))

    (let (main-flow
          frame-start)
      (setf main-flow
            (>> (instantly ()
                  (setf frame-start (real-time-seconds))
                  (clear-links reg))
                (-> ((physics)) ()
                  (observe-universe 0.014))
                (-> this ()
                  (make-chains reg)
                  (strike ctx))
                (scene-processing-flow scene)
                (instantly ()
                  (update-hud ctx)
                  (update-averaging-gauge (ctx-metrics ctx)
                                          :frame-time
                                          (- (real-time-seconds) frame-start)))
                (concurrently ()
                  (when (enabledp this)
                    (run main-flow)))))
      (run main-flow))))


(defmethod make-system-context ((this ball-z))
  (let ((ctx (make-ball-z-context (make-default-keymap)
                                  (make-instance 'chain-registry))))
    (run (>> (make-main-scenegraph ctx)
             (-> (this) (scenegraph)
               (let ((pass-chain (make-pass-chain (make-simulation-pass)
                                                  (make-rendering-pass))))
                 (setf (ctx-scene ctx) (make-scene pass-chain scenegraph))
                 (init-context this ctx)))))
    ctx))


(defmethod destroy-system-context ((this ball-z) ctx)
  (stop-background-music ctx)
  (dispose (ctx-scene ctx)))


(defun start (configuration-pathname)
  (log:config :sane2)
  (startup configuration-pathname))


(defun main (args)
  (start (merge-pathnames (second args) (uiop:getcwd)))
  (wait-for-latch *main-latch*))
