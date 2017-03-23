(in-package :ball-z)


(defclass hud-node (scene-node)
  ((score-text :initform (string->mutable "SCORE 0000"))
   (timer-text :initform (string->mutable "TIMER 03:00"))
   (5by7-renderer :initform nil)))


(defmethod initialization-flow ((this hud-node) &key)
  (with-slots (5by7-renderer) this
    (>> (call-next-method)
        (asset-flow (font-asset-id "5by7.ttf"))
        (-> ((graphics)) (5by7-font)
          (setf 5by7-renderer (make-text-renderer 640 480 5by7-font 18))))))


(defmethod scene-pass ((this hud-node) (pass rendering-pass) input)
  (with-slots (5by7-renderer score-text timer-text) this
    (draw-text 5by7-renderer score-text :position (vec2 0 (- 480 20)))
    (draw-text 5by7-renderer timer-text :position (vec2 (- 320 64) (- 480 20))))
  (call-next-method))


(defun update-score-text (node score)
  (with-slots (score-text) node
    (mutate-string score-text "SCORE ~4,'0d" score)))


(defun update-timer-text (node min sec)
  (with-slots (timer-text) node
    (mutate-string timer-text "TIMER ~2,'0d:~2,'0d" min sec)))
