(in-package :ball-z)

;;
;; todo: make thread safe?
;;
(defclass chain-registry ()
  ((b-geoms :initform (tg:make-weak-hash-table :weakness :key))
   (links :initform '())
   (geom-chain :initform (make-hash-table :test 'eq))
   (chains :initform (make-hash-table :test 'eql))))


(defun register-bounding-geom (registry geom group)
  (with-slots (b-geoms) registry
    (setf (gethash geom b-geoms) group)))


(defun register-chain-link (reg this-geom that-geom)
  (with-slots (links) reg
    (push (cons this-geom that-geom) links)))


(defun process-collision (reg this-geom that-geom)
  (with-slots (b-geoms) reg
    (with-hash-entries ((this-group this-geom) (that-group that-geom)) b-geoms
      (when (or this-group that-group)
        (when (eq this-group that-group)
          (register-chain-link reg this-geom that-geom))
        t))))


(defun clear-links (reg)
  (with-slots (links chains geom-chain) reg
    (setf links '())
    (clrhash geom-chain)
    (clrhash chains)))


(defun linked-p (reg geom)
  (with-slots (geom-chain) reg
    (not (null (gethash geom geom-chain)))))


(defun make-chains (reg)
  (with-slots (links chains geom-chain) reg
    (let ((links links)
          (chains chains)
          (geom-chain geom-chain))
      (flet ((%make-chain (id g0 g1)
               (setf (gethash id chains) (list g0 g1)
                     (gethash g0 geom-chain) id
                     (gethash g1 geom-chain) id))
             (%add-link (id geom)
               (push geom (gethash id chains))
               (setf (gethash geom geom-chain) id))
             (%merge-chains (id0 id1)
               (let ((geoms (gethash id1 chains)))
                 (nconcf (gethash id0 chains) geoms)
                 (remhash id1 chains)
                 (dolist (g geoms)
                   (setf (gethash g geom-chain) id0)))))
        (loop with last-chain-id = 0
           for (g0 . g1) in links do
             (with-hash-entries ((g0-chain-id g0) (g1-chain-id g1)) geom-chain
               (let ((ch0-id g0-chain-id)
                     (ch1-id g1-chain-id))
                 (unless (and ch0-id (eql ch0-id ch1-id))
                   (cond
                     ((and (null ch0-id) (null ch1-id))
                      (%make-chain (incf last-chain-id) g0 g1))
                     ((null ch0-id) (%add-link ch1-id g0))
                     ((null ch1-id) (%add-link ch0-id g1))
                     (t (%merge-chains ch0-id ch1-id))))))
           finally (return chains))))))
