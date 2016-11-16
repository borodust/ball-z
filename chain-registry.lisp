(in-package :ball-z)

;;
;; todo: make thread safe?
;;
(defclass chain-registry ()
  ((b-geoms :initform (tg:make-weak-hash-table :weakness :key))))


(defun register-bounding-geom (registry geom)
  (with-slots (b-geoms) registry
    (setf (gethash geom b-geoms) geom)))


(defun geom-present-p (registry geom)
  (with-slots (b-geoms) registry
    (not (null (gethash geom b-geoms)))))
