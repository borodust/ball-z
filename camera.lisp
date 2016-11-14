(in-package :ball-z)


(defparameter *camera-translation* (translation-mat4 0.0 -10.0 -10.0))

;;;
;;;
;;;
(defclass player-camera-node (node)
  ((circle-angle :initform 0.0)
   (pitch-angle :initform 0.0)
   (transform :initform *camera-translation*)))


(defmethod rendering-pass ((this player-camera-node))
  (with-slots (transform) this
    (let ((*transform-matrix* (mult *transform-matrix* transform)))
      (call-next-method))))


(defun %cache-transform (cam)
  (with-slots (transform circle-angle pitch-angle) cam
    (setf transform (mult (euler-axis->mat4 #f pitch-angle (vec3 1.0 0.0 0.0))
                          *camera-translation*
                          (euler-axis->mat4 #f circle-angle (vec3 0.0 1.0 0.0))))))


(defun move-camera (cam angle)
  (with-slots (circle-angle) cam
    (decf circle-angle angle))
  (%cache-transform cam))


(defun pitch-camera (cam angle)
  (with-slots (pitch-angle) cam
    (let ((new-angle (max (- (/ +half-pi+ 2)) (min (/ +half-pi+ 2) (+ pitch-angle angle)))))
      (unless (= new-angle pitch-angle)
        (setf pitch-angle new-angle)
        (%cache-transform cam)))))
