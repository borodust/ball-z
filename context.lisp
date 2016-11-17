(in-package :ball-z)


(defun make-default-keymap (registry)
  (ge.util:make-hash-table-with-entries () ((w :w) (a :a) (s :s) (d :d) (m :m)
                                            (space :space))
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
          space (lambda (s)
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
                    (adopt place new-ball))))))

(defstruct (ball-z-ctx
             (:conc-name ctx-)
             (:constructor %make-ball-z-context (scene keymap event-system chain-registry)))
  (scene nil :read-only t)
  (keymap nil :read-only t)
  (event-system nil :read-only t)
  (chain-registry nil :read-only t)
  (strike nil))


(defun make-ball-z-context (scene ball-z event-system)
  (let ((reg (make-instance 'chain-registry)))
    (%make-ball-z-context scene (make-default-keymap reg) event-system reg)))


(defun bind-key (key action)
  (with-slots (keymap) *system-context*
    (setf (gethash key keymap) action)))


(defun register-strike (ctx b0 b1)
  (setf (ctx-strike ctx) (cons b0 b1)))
