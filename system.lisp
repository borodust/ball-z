(in-package :ball-z)


(defvar *main-latch* (make-latch))


(defclass ball-z (thread-bound-system)
  ((events :initform nil :reader event-system-of)
   (physics :initform nil)
   (audio :initform nil))
  (:default-initargs :depends-on '(host-system
                                   graphics-system
                                   physics-system
                                   audio-system
                                   resource-system)))


(defun exit-game ()
  (-> ((engine))
    (shutdown)
    (log:debug "Ball-Z stopped")
    (open-latch *main-latch*)))


(defmethod initialize-system :after ((this ball-z))
  (with-slots (events physics audio) this
    (log:config (property :log-level :info))
    (let ((host (engine-system 'host-system)))
      (setf physics (engine-system 'physics-system))
      (setf events (engine-system 'event-system))
      (setf audio (engine-system 'audio-system))
      (register-event-classes events
                              'chain-broke-event
                              'game-loaded-event
                              'game-started-event
                              'game-ended-event)

      (when-all ((-> (host)
                   (setf (viewport-title host) "Ball-Z")
                   (lock-cursor host))
                 (-> (physics)
                   (setf (gravity) (vec3 0.0 -9.81 0.0))))

        (subscribe-with-handler-body-to viewport-hiding-event () events
          (exit-game))
        (log:debug "Ball-Z started")))))


(defun strike (ctx)
  (when-let ((strike (pop-strike ctx)))
    (let ((reg (ctx-chain-registry ctx))
          (events (ctx-event-system ctx)))
      (destructuring-bind (b0 . b1) strike
        (process-strike b0 b1 reg events)))))


(defun throw-ball-action (registry)
  (lambda (s)
    (let* ((place (node s :place))
           (balls (node s :balls))
           (ball (first (children-of place)))
           (new-ball (make-instance 'ball-model :simulated-p nil
                                    :chain-registry registry)))
      (mt:wait-with-latch (l)
        (alet ((nil (initialize-tree s new-ball)))
          (open-latch l)))
      (abandon place ball)
      (adopt balls ball)
      (throw-ball ball)
      (adopt place new-ball))))


(defun start-game-action (sys reg)
  (lambda (s)
    (-> (sys :high)
      (disable-node (node s :start-screen))
      (bind-key :space (throw-ball-action reg))
      (post (make-instance 'game-started-event) (event-system-of sys)))))


(defun generate-balls (ctx)
  (let* ((s (ctx-scene ctx))
         (registry (ctx-chain-registry ctx))
         (balls (node s :balls)))
    (flet ((%gen ()
             (bt:make-thread
              (lambda ()
                (loop repeat 64 do
                     (let ((b (make-ball registry)))
                       (when-all ((initialize-tree s b))
                         (adopt balls b)
                         (push-ball b (mult (normalize (vec3 (- (random 2.0) 1.0)
                                                             (random 1.0)
                                                             (- (random 2.0) 1.0)))
                                            (random 100.0))))
                       (sleep 0.1)))))))
      (%gen)
      (%gen)
     :name "ball-gen")))


(defun remove-balls (ctx)
  (let* ((s (ctx-scene ctx))
         (balls (node s :balls)))
    (loop for ball in (children-of balls) do
         (abandon balls ball)
         (discard-node ball))))


(defun display-score (ctx)
  (let ((score-node (node (ctx-scene ctx) :score)))
    (setf (text-of score-node) (format nil "SCORE ~4,'0d" (ctx-score ctx)))))


(defun update-timer (ctx)
  (let* ((timer-node (node (ctx-scene ctx) :timer))
         (started-at (ctx-timer-started ctx))
         (now (floor (epoch-seconds)))
         (max (* 3 60))
         (passed (- now (floor (or started-at 0))))
         (left (max 0 (- max passed)))
         (min (if started-at (floor (/ left 60)) 3))
         (sec (if started-at (mod left 60) 0)))
    (when (and started-at (> passed max))
      (reset-timer ctx)
      (post (make-instance 'game-ended-event) (ctx-event-system ctx)))
    (setf (text-of timer-node) (format nil "TIMER ~2,'0d:~2,'0d" min sec))))


(defun make-default-keymap ()
  (ge.util:make-hash-table-with-entries () ((w :w) (a :a) (s :s) (d :d) (m :m)
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
          m (lambda (s)
              (let ((frame (node s :wireframe)))
                (setf (enabledp frame) (not (enabledp frame)))))
          esc (lambda (s)
                (declare (ignore s))
                (exit-game)))))


(defmethod make-system-context ((this ball-z))
  (with-slots (events physics audio) this
    (let* ((scene (make-main-scene))
           (cam (node scene :camera))
           (reg (make-instance 'chain-registry))
           (ctx (make-ball-z-context scene (make-default-keymap) reg
                                     events audio)))

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

      (let ((prev-x nil)
            (prev-y nil))
        (subscribe-with-handler-body-to cursor-event (ev) events
          (when (and prev-x prev-y)
            (let* ((y (* (- (x-from ev) prev-x) 0.006))
                   (x (* (- (y-from ev) prev-y) 0.0025)))
              (pitch-camera cam x)
              (move-camera cam y)))
          (setf prev-x (x-from ev)
                prev-y (y-from ev))))


      (subscribe-with-handler-body-to mouse-event (ev) events
        (-> (this :high)
          (when (and (eq :pressed (state-from ev))
                     (eq :left (button-from ev)))
            (execute-key-action ctx :space))))

      (subscribe-with-handler-body-to game-loaded-event () events
        (-> (this)
          (disable-node (node scene :loading-text))
          (enable-node (node scene :start-text))
          (bind-key :enter (start-game-action this reg))))

      (adopt (node scene :place)
             (make-instance 'ball-model :simulated-p nil
                            :chain-registry (ctx-chain-registry ctx)))

      (-> (physics)
        (register-collision-callback
         (lambda (g0 g1)
           (process-bounds-collision reg g0 g1)))

        (register-contact-callback
         (lambda (g0 g1)
           (let ((reg (ctx-chain-registry ctx)))
             (when-let ((b0 (find-model-by-geom reg g0))
                        (b1 (find-model-by-geom reg g1)))
               (cond
                 ((virginp b0)
                  (register-strike ctx b0 b1))
                 ((virginp b1)
                  (register-strike ctx b1 b0)))
               nil)))))

      (subscribe-with-handler-body-to chain-broke-event (ev) events
        (with-accessors ((balls balls-from)) ev
          (-> (this)
            (update-score ctx (* (length balls) 10))
            (dolist (b balls)
              (abandon (parent-of b) b)
              (discard-node b)))))

      (subscribe-with-handler-body-to game-started-event () events
        (-> (this :high)
          (remove-balls ctx)
          (reset-score ctx)
          (start-timer ctx)
          (generate-balls ctx)))

      (subscribe-with-handler-body-to game-ended-event () events
        (-> (this :high)
          (reset-timer ctx)
          (bind-key :space (lambda (s) (declare (ignore s))))
          (enable-node (node scene :start-screen))))

      (load-background-music ctx)

      (bt:make-thread
       (lambda ()
         (let ((reg (ctx-chain-registry ctx)))
           (loop while (enabledp this) do
                (-> (physics)
                  (clear-links reg)
                  (observe-universe 0.014)
                  (make-chains reg)
                  (strike ctx))
                (animate scene)
                (update-timer ctx)
                (display-score ctx)
                (sleep 0.012))))
       :name "scene-worker")
      ctx)))


(defmethod destroy-system-context (ctx (this ball-z))
  (stop-background-music ctx)
  (dispose (ctx-scene ctx)))


(defun start (&optional (working-directory *default-pathname-defaults*))
  (log:config :sane2)
  (startup (setf *configuration-pathname*
                 (fad:merge-pathnames-as-file working-directory "ball-z.conf"))))


(defun main (&rest args)
  (declare (ignore args))
  (start)
  (wait-for-latch *main-latch*))
