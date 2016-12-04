(in-package :ball-z)

;;;
;;;
;;;
(defclass stage-mesh (mesh-node)
  ((transform :initform (identity-mat4))
   (geoms :initform '())))


(defmethod initialize-node :after ((this stage-mesh) (system physics-system))
  (with-slots (geoms) this
    (setf geoms (list
                 (make-plane-geom 0.0 0.9803922 -0.19607845 -2.0)
                 (make-plane-geom 0.19607845 0.9803922 0.0 -2.0)
                 (make-plane-geom 0.0 0.9803922 0.19607845 -2.0)
                 (make-plane-geom -0.19607845 0.9803922 0.0 -2.0)

                 (make-plane-geom 0.0 0.0 1.0 -10.0)
                 (make-plane-geom 0.0 0.0 -1.0 -10.0)
                 (make-plane-geom 1.0 0.0 0.0 -10.0)
                 (make-plane-geom -1.0 0.0 0.0 -10.0)))))


(defmethod discard-node :before ((this stage-mesh))
  (with-slots (geoms) this
    (dolist (g geoms)
      (dispose g))))


(defmethod make-node-mesh ((this stage-mesh) system)
  (let ((mesh (make-mesh 12 :triangles)))
    (with-disposable ((vbuf (make-array-buffer
                             (make-array '(12 3) :element-type 'single-float
                                         :initial-contents
                                         #((0.0 -2.0 0.0)
                                           (-10.0 0.0 10.0)
                                           (10.0 0.0 10.0)

                                           (0.0 -2.0 0.0)
                                           (-10.0 0.0 -10.0)
                                           (-10.0 0.0 10.0)

                                           (0.0 -2.0 0.0)
                                           (10.0 0.0 -10.0)
                                           (-10.0 0.0 -10.0)

                                           (0.0 -2.0 0.0)
                                           (10.0 0.0 10.0)
                                           (10.0 0.0 -10.0)))))
                      (nbuf (make-array-buffer
                             (make-array '(12 3) :element-type 'single-float
                                         :initial-contents
                                         #((0.0 0.9803922 -0.19607845)
                                           (0.0 0.9803922 -0.19607845)
                                           (0.0 0.9803922 -0.19607845)

                                           (0.19607845 0.9803922 0.0)
                                           (0.19607845 0.9803922 0.0)
                                           (0.19607845 0.9803922 0.0)

                                           (0.0 0.9803922 0.19607845)
                                           (0.0 0.9803922 0.19607845)
                                           (0.0 0.9803922 0.19607845)

                                           (-0.19607845 0.9803922 0.0)
                                           (-0.19607845 0.9803922 0.0)
                                           (-0.19607845 0.9803922 0.0))))))
      (attach-array-buffer vbuf mesh 0)
      (attach-array-buffer nbuf mesh 1))
    mesh))


(defmethod scene-pass ((this stage-mesh) (pass rendering-pass) input)
  (with-slots (transform) this
    (let ((*transform-matrix* (mult *transform-matrix* transform)))
      (setf (shading-parameter "normalTransform") (mat4->mat3 *transform-matrix*)
            (shading-parameter "modelViewProjection") (mult *projection-matrix*
                                                            (transform-of *camera*)
                                                            *transform-matrix*)
            (shading-parameter "dLight.ambient") (vec3 0.2 0.2 0.2)
            (shading-parameter "dLight.diffuse") (vec3 0.8 0.8 0.8)
            (shading-parameter "dLight.direction") (vec3 -0.57735026 -0.57735026 -0.57735026))
        (setf (shading-parameter "baseColor") (vec3 0.8 0.8 0.8))
      (call-next-method))))
