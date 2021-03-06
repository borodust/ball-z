(in-package :ball-z)


(defstruct (ball-z-ctx
             (:conc-name ctx-)
             (:constructor make-ball-z-context (keymap chain-registry)))
  (keymap nil :read-only t)
  (chain-registry nil :read-only t)
  (metrics (make-metrics-registry) :read-only t)
  (background-audio nil)
  (strike nil)
  (score 0)
  (timer-started nil)
  (scene nil))


(defun bind-key (key action)
  (with-slots (keymap) *system-context*
    (setf (gethash key keymap) action)))


(defun register-strike (ctx b0 b1)
  (unless (ctx-strike ctx)
    (setf (ctx-strike ctx) (cons b0 b1))))


(defun pop-strike (ctx)
  (prog1 (ctx-strike ctx)
    (setf (ctx-strike ctx) nil)))


(defun reset-score (ctx)
  (setf (ctx-score ctx) 0))


(defun update-score (ctx diff)
  (incf (ctx-score ctx) diff))


(defun start-timer (ctx)
  (setf (ctx-timer-started ctx) (epoch-seconds)))


(defun reset-timer (ctx)
  (setf (ctx-timer-started ctx) nil))


(defun execute-key-action (ctx key)
  (when-let ((action (gethash key (ctx-keymap ctx))))
    (funcall action (ctx-scene ctx))))
