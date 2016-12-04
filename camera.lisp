(in-package :ball-z)


(defparameter *camera-translation* (translation-mat4 0.0 -10.0 -10.0))
(declaim (special *camera*))
;;;
;;;
;;;
(defclass camera ()
  ((transform :initform nil :reader transform-of)
   (position :initform nil :reader position-of)
   (orientation :initform nil :reader orientation-of)))


(defun update-camera (cam transform)
  (with-slots ((ctm transform) position orientation) cam
    (let ((inverse (inverse transform)))
      (setf ctm transform
            position (make-vec3 (mult inverse (vec4 0.0 0.0 0.0 1.0)))
            orientation (normalize (mult (make-mat3 inverse)
                                         (vec3 0.0 0.0 -1.0)))))))


(defclass player-camera-node (scene-node)
  ((circle-angle :initform 0.0)
   (pitch-angle :initform (/ +half-pi+ 4))
   (camera :initform (make-instance 'camera))
   (transform :initform *camera-translation* :reader transform-of)))


(defun %cache-transform (cam)
  (with-slots (transform circle-angle pitch-angle) cam
    (setf transform (mult (euler-axis->mat4 #f pitch-angle (vec3 1.0 0.0 0.0))
                          *camera-translation*
                          (euler-axis->mat4 #f circle-angle (vec3 0.0 1.0 0.0))))))


(defmethod initialize-instance :after ((this player-camera-node) &key)
  (%cache-transform this))


(defmethod scene-pass ((this player-camera-node) (pass rendering-pass) input)
  (with-slots (transform camera) this
    (update-camera camera transform)
    (let ((*camera* camera))
      (call-next-method))))


(defun move-camera (cam angle)
  (with-slots (circle-angle) cam
    (decf circle-angle angle))
  (%cache-transform cam))


(defun pitch-camera (cam angle)
  (with-slots (pitch-angle) cam
    (let ((new-angle (max (/ +half-pi+ 8) (min (/ +half-pi+ 2) (+ pitch-angle angle)))))
      (unless (= new-angle pitch-angle)
        (setf pitch-angle new-angle)
        (%cache-transform cam)))))
