(in-package :ball-z)

;;
;; todo: make thread safe?
;;
(defclass chain-registry ()
  ((b-geoms :initform (tg:make-weak-hash-table :weakness :key))
   (links :initform '())
   (chains :initform '())))


(defun register-bounding-geom (registry geom group)
  (with-slots (b-geoms) registry
    (setf (gethash geom b-geoms) group)))


(defun register-chain-link (reg this-geom that-geom)
  (with-slots (links) reg
    (push (cons this-geom that-geom) links)
    (push (cons that-geom this-geom) links)))


(defun process-collision (reg this-geom that-geom)
  (with-slots (b-geoms) reg
    (with-hash-entries ((this-group this-geom) (that-group that-geom)) b-geoms
      (when (or this-group that-group)
        (when (eq this-group that-group)
          (register-chain-link reg this-geom that-geom))
        t))))


(defun clear-links (reg)
  (with-slots (links chains) reg
    (setf links '()
          chains '())))


(defun linked-p (reg geom)
  (with-slots (links) reg
    (not (null (assoc geom links)))))


(defun find-chains (reg)
  (declare (ignore reg)))
