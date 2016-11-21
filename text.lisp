(in-package :ball-z)


(defclass text-node (node)
  ((tex-atlas :initform nil)
   (mesh :initform nil)
   (program :initform nil)
   (proj :initform (orthographic-projection-mat 640.0 480.0 1.0 -1.0))
   (pos :initform (vec2))
   (font :initarg :font :initform "5by7")
   (text :initform "SCORE 0000")))


(defun %build-texting-program (system)
  (make-shading-program
   system
   (load-shader-source :vertex-shader (resource-truename "shaders/v_text.glsl"))
   (load-shader-source :fragment-shader (resource-truename "shaders/f_text.glsl"))))


(defun %set-text-vertex-data (arr i x0 y0 x1 y1)
  (flet ((%setv (v x y)
           (setf (aref arr v 0) #f x
                 (aref arr v 1) #f y)))
    (let* ((v0 (* i 4))
           (v1 (+ v0 1))
           (v2 (+ v0 2))
           (v3 (+ v0 3)))
      (%setv v0 x0 y0)
      (%setv v1 x0 y1)
      (%setv v2 x1 y1)
      (%setv v3 x1 y0))))


(defun %set-index-data (arr i)
  (let* ((quad (1- (* i 6)))
         (vert (* i 4)))
    (flet ((%seti (i v)
             (setf (aref arr i) v)))
      (%seti (incf quad) (+ 0 vert))
      (%seti (incf quad) (+ 1 vert))
      (%seti (incf quad) (+ 2 vert))
      (%seti (incf quad) (+ 2 vert))
      (%seti (incf quad) (+ 3 vert))
      (%seti (incf quad) (+ 0 vert)))))


(defmethod initialize-node :after ((this text-node) (system graphics-system))
  (with-slots (tex-atlas mesh text program font) this
    (let ((len (length text)))
      (setf tex-atlas (make-2d-texture system (load-png-image
                                               (resource-truename
                                                (format nil "fonts/~a.png" font)))
                                       :grey)
            program (%build-texting-program system))
      (let* ((i 0)
             (vertex-count (* len 4))
             (tex-array (make-array (list vertex-count 2) :element-type 'single-float))
             (pos-array (make-array (list vertex-count 2) :element-type 'single-float))
             (index-array (make-array (* len 6) :element-type 'integer)))
        (texatl.cl:do-texatl-string (text
                                     x0 y0 x1 y1 u0 v0 u1 v1 :tex-width 256 :tex-height 256)
            (first (conspack:decode-file (resource-truename
                                          (format nil "fonts/~a.met" font))))
          (%set-text-vertex-data pos-array i x0 (- y0) x1 (- y1))
          (%set-text-vertex-data tex-array i u0 (- 1.0 v0) u1 (- 1.0 v1))
          (%set-index-data index-array i)
          (incf i))
        (setf mesh (make-mesh system vertex-count :triangles index-array))
        (with-disposable ((vbuf (make-array-buffer system 0 pos-array))
                          (tbuf (make-array-buffer system 1 tex-array)))
          (attach-gpu-buffer vbuf mesh)
          (attach-gpu-buffer tbuf mesh))))))


(defmethod discard-node :before ((this text-node))
  (with-slots (tex-atlas mesh) this
    (dispose mesh)
    (dispose tex-atlas)))


(defmethod rendering-pass ((this text-node))
  (with-slots (tex-atlas mesh program proj pos) this
    (with-using-shading-program (program)
      (setf (program-uniform-variable program "atlas") 0
            (program-uniform-variable program "proj") proj
            (program-uniform-variable program "pos") (vec2 -320.0 240.0))
      (with-bound-texture (tex-atlas)
        (render mesh))))
  (call-next-method))
