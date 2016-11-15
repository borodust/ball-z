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
                 (make-plane-geom system 0.0 0.99502474 -0.099502474 -1.0)
                 (make-plane-geom system 0.099502474 0.99502474 0.0 -1.0)
                 (make-plane-geom system 0.0 0.99502474 0.099502474 -1.0)
                 (make-plane-geom system -0.099502474 0.99502474 0.0 -1.0)))))


(defmethod discard-node :before ((this stage-mesh))
  (with-slots (geoms) this
    (dolist (g geoms)
      (dispose g))))


(defmethod make-node-mesh ((this stage-mesh) system)
  (let ((mesh (make-mesh system 12 :triangles)))
    (with-disposable ((vbuf (make-array-buffer
                             system 0 (make-array '(12 3) :element-type 'single-float
                                                  :initial-contents
                                                  #((0.0 -1.0 0.0)
                                                    (-10.0 0.0 10.0)
                                                    (10.0 0.0 10.0)

                                                    (0.0 -1.0 0.0)
                                                    (-10.0 0.0 -10.0)
                                                    (-10.0 0.0 10.0)

                                                    (0.0 -1.0 0.0)
                                                    (10.0 0.0 -10.0)
                                                    (-10.0 0.0 -10.0)

                                                    (0.0 -1.0 0.0)
                                                    (10.0 0.0 10.0)
                                                    (10.0 0.0 -10.0)))))
                      (nbuf (make-array-buffer
                             system 1 (make-array '(12 3) :element-type 'single-float
                                                  :initial-contents
                                                  #((0.0 0.99502474 -0.099502474)
                                                    (0.0 0.99502474 -0.099502474)
                                                    (0.0 0.99502474 -0.099502474)

                                                    (0.099502474 0.99502474 0.0)
                                                    (0.099502474 0.99502474 0.0)
                                                    (0.099502474 0.99502474 0.0)

                                                    (0.0 0.99502474 0.099502474)
                                                    (0.0 0.99502474 0.099502474)
                                                    (0.0 0.99502474 0.099502474)

                                                    (-0.099502474 0.99502474 0.0)
                                                    (-0.099502474 0.99502474 0.0)
                                                    (-0.099502474 0.99502474 0.0))))))
      (attach-gpu-buffer vbuf mesh)
      (attach-gpu-buffer nbuf mesh))
    mesh))


(defmethod rendering-pass ((this stage-mesh))
  (with-slots (transform) this
    (let ((*transform-matrix* (mult *transform-matrix* transform)))
      (setf (shading-parameter "normalTransform") (mat4->mat3 *transform-matrix*)
            (shading-parameter "modelViewProjection") (mult *projection-matrix*
                                                            *transform-matrix*)
            (shading-parameter "dLight.ambient") (vec3 0.2 0.2 0.2)
            (shading-parameter "dLight.diffuse") (vec3 0.8 0.8 0.8)
            (shading-parameter "dLight.direction") (vec3 -0.57735026 -0.57735026 -0.57735026))
      (call-next-method))))
