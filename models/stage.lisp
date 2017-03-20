(in-package :ball-z)

;;;
;;;
;;;
(defclass stage-mesh (mesh-node)
  ((transform :initform (identity-mat4))
   (geoms :initform '())))


(defmethod initialization-flow ((this stage-mesh) &key)
  (with-slots (geoms) this
    (>> (call-next-method)
        (-> ((physics)) ()
          (setf geoms (list
                       (make-instance 'plane-geom :normal (vec3 0.0 0.9803922 -0.19607845)
                                      :offset -2.0)
                       (make-instance 'plane-geom :normal (vec3 0.19607845 0.9803922 0.0)
                                      :offset -2.0)
                       (make-instance 'plane-geom :normal (vec3 0.0 0.9803922 0.19607845)
                                      :offset -2.0)
                       (make-instance 'plane-geom :normal (vec3 -0.19607845 0.9803922 0.0)
                                      :offset -2.0)

                       (make-instance 'plane-geom :normal (vec3 0.0 0.0 1.0)
                                      :offset -10.0)
                       (make-instance 'plane-geom :normal (vec3 0.0 0.0 -1.0)
                                      :offset -10.0)
                       (make-instance 'plane-geom :normal (vec3 1.0 0.0 0.0)
                                      :offset -10.0)
                       (make-instance 'plane-geom :normal (vec3 -1.0 0.0 0.0)
                                      :offset -10.0)))))))


(defmethod collide ((this plane-geom) that)
  nil)


(defmethod collide (this (that plane-geom))
  (collide that this))


(defmethod discard-node :before ((this stage-mesh))
  (with-slots (geoms) this
    (dolist (g geoms)
      (dispose g))))


(defmethod make-node-mesh ((this stage-mesh))
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
    (let ((*model-matrix* (mult *model-matrix* transform)))
      (setf (shading-parameter "normalTransform") (mat4->mat3 *model-matrix*)
            (shading-parameter "modelViewProjection") (mult *projection-matrix*
                                                            (transform-of *camera*)
                                                            *model-matrix*)
            (shading-parameter "dLight.ambient") (vec4 0.2 0.2 0.2 1.0)
            (shading-parameter "dLight.diffuse") (vec4 0.8 0.8 0.8 1.0)
            (shading-parameter "dLight.direction") (vec3 -0.57735026 -0.57735026 -0.57735026))
        (setf (shading-parameter "baseColor") (vec4 0.8 0.8 0.8 1.0))
      (call-next-method))))
