(in-package :ball-z)


(defclass start-screen-node (enableable-node scene-node)
  ((title-tex :initform nil)
   (banner :initform nil)
   (program :initform nil)
   (proj :initform (orthographic-projection-mat 640.0 480.0 1.0 -1.0))))


(defun %build-banner-program ()
  (make-shading-program
   (list
    (load-shader-source :vertex-shader (resource-truename "shaders/v_banner.glsl"))
    (load-shader-source :fragment-shader (resource-truename "shaders/f_banner.glsl")))))


(defmethod initialize-node ((this start-screen-node) (system graphics-system))
  (with-slots (title-tex banner program) this
    (setf title-tex (make-2d-texture (load-png-image
                                      (resource-truename "images/ball-z.png"))
                                     :rgba)
          banner (make-mesh 4 :triangle-strip)
          program (%build-banner-program))
    (with-disposable ((vbuf (make-array-buffer #2a((439.0 0.0)
                                                   (439.0 105.0)
                                                   (0.0 0.0)
                                                   (0.0 105.0))))
                      (tbuf (make-array-buffer #2a((1.0 0.0)
                                                   (1.0 1.0)
                                                   (0.0 0.0)
                                                   (0.0 1.0)))))
      (attach-array-buffer vbuf banner 0)
      (attach-array-buffer tbuf banner 1))))


(defmethod scene-pass ((this start-screen-node) (pass rendering-pass) input)
  (with-slots (title-tex banner proj program) this
    (with-using-shading-program (program)
      (setf (program-uniform-variable program "proj") proj
            (program-uniform-variable program "pos") (vec2 -219.5 64.0)
            (program-uniform-variable program "banner") 0)
      (with-bound-texture (title-tex)
        (render banner))))
  (call-next-method))
