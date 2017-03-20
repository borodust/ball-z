(in-package :ball-z)


(defclass debug-node (scene-node)
  ((frame-time-text :initform (string->mutable "FRAME: 00.0000"))
   (metrics :initarg :metrics :initform (error ":metrics missing"))
   (5by7-renderer :initform nil)))


(defmethod initialization-flow ((this debug-node) &key)
  (with-slots (5by7-renderer) this
    (>> (call-next-method)
        (asset-flow (font-asset-id "5by7.ttf"))
        (-> ((graphics)) (5by7-font)
          (setf 5by7-renderer (make-text-renderer 640 480 5by7-font 18))))))


(defun update-frame-time-text (node)
  (with-slots (frame-time-text metrics) node
    (mutate-string frame-time-text
                   "FRAME: ~7,4,,,'0f"
                   (averaging-gauge-value metrics :frame-time))))


(defmethod scene-pass ((this debug-node) (pass rendering-pass) input)
  (with-slots (5by7-renderer frame-time-text) this
    (update-frame-time-text this)
    (draw-text 5by7-renderer frame-time-text :position (vec2 (- 320 100) (- 480 60))))
  (call-next-method))
