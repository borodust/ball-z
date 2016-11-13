(in-package :ball-z)


(defclass ball-z (enableable generic-system) ()
  (:default-initargs :depends-on '(host-system
                                   graphics-system
                                   physics-system
                                   audio-system
                                   resource-system)))


(defmethod initialize-system :after ((this ball-z))
  (log:config (property :log-level :info))
  (let ((host (engine-system 'host-system)))
    (when-all ((-> host
                 (setf (viewport-title host) "Ball-Z")))
      (log:debug "Ball-Z initialized"))))


(defun start (&optional (working-directory *default-pathname-defaults*))
  (log:config :sane2)
  (startup (fad:merge-pathnames-as-file working-directory "ball-z.conf")))
