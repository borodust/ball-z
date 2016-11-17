(in-package :ball-z)



(defstruct (ball-z-ctx
             (:conc-name ctx-)
             (:constructor %make-ball-z-context (scene keymap event-system chain-registry)))
  (scene nil :read-only t)
  (keymap nil :read-only t)
  (event-system nil :read-only t)
  (chain-registry nil :read-only t)
  (strike nil))


(defun make-ball-z-context (scene event-system keymap reg)
  (%make-ball-z-context scene keymap event-system reg))


(defun bind-key (key action)
  (with-slots (keymap) *system-context*
    (setf (gethash key keymap) action)))


(defun register-strike (ctx b0 b1)
  (setf (ctx-strike ctx) (cons b0 b1)))
