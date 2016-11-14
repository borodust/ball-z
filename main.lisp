(in-package :ball-z)


(defvar *main-latch* (make-latch))


(defclass ball-z (enableable generic-system)
  ((keymap :initform nil))
  (:default-initargs :depends-on '(host-system
                                   graphics-system
                                   physics-system
                                   audio-system
                                   resource-system)))

(defun bind-key (key action)
  (with-slots (keymap) (engine-system 'ball-z)
    (setf (gethash key keymap) action)))


(defun make-default-keymap ()
  (ge.util:make-hash-table-with-entries () ((w :w) (a :a) (s :s) (d :d))
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
                (move-camera cam 0.1))))))


(defmethod initialize-system :after ((this ball-z))
  (with-slots (keymap) this
    (log:config (property :log-level :info))
    (let ((host (engine-system 'host-system))
          (events (engine-system 'event-system)))
      (setf keymap (make-default-keymap))

      (when-all ((-> host
                   (setf (viewport-title host) "Ball-Z")))
        (subscribe-with-handler-body-to viewport-hiding-event () events
          (-> (engine)
            (shutdown)
            (log:debug "Ball-Z stopped")
            (open-latch *main-latch*)))
        (let* ((scene (make-main-scene))
               (cam (node scene :camera)))
          (subscribe-with-handler-body-to keyboard-event (ev) events
            (ge.util:with-hash-entries ((key-fn (key-from ev))) keymap
              (let ((fn key-fn))
                (when (and fn (eq :pressed (state-from ev)))
                  (funcall fn scene)))))
          (subscribe-with-handler-body-to scroll-event (ev) events
            (let* ((y (* (x-offset-from ev) 0.005))
                   (x (* (y-offset-from ev) 0.005)))
              (pitch-camera cam x)
              (move-camera cam y)))
          (bt:make-thread
           (lambda ()
             (loop while (enabledp this) do
                  (animate scene)
                  (sleep 0.014))))
          :name "scene-worker")
        (log:debug "Ball-Z started")))))


(defun start (&optional (working-directory *default-pathname-defaults*))
  (log:config :sane2)
  (startup (setf *configuration-pathname*
                 (fad:merge-pathnames-as-file working-directory "ball-z.conf"))))


(defun main (&rest args)
  (declare (ignore args))
  (start)
  (wait-for-latch *main-latch*))