(in-package :ball-z)


(defclass metrics-registry (lockable)
  ((values :initform (make-hash-table :test #'equal))
   (averages :initform (make-hash-table :test #'equal))))


(defun make-metrics-registry ()
  (make-instance 'metrics-registry))


(defun gauge-value (registry metric-name)
  (with-instance-lock-held (registry)
    (with-slots (values) registry
      (gethash metric-name values))))


(defun update-gauge (registry metric-name value)
  (with-instance-lock-held (registry)
    (with-slots (values) registry
      (setf (gethash metric-name values) value))))


(defun averaging-gauge-value (registry metric-name)
  (with-instance-lock-held (registry)
    (with-slots (averages) registry
      (cdr (gethash metric-name averages)))))


(defun average (current-average next-val next-n)
  (+ current-average (/ (- next-val current-average) next-n)))


(defun update-averaging-gauge (registry metric-name value)
  (with-instance-lock-held (registry)
    (with-slots (averages) registry
      (let* ((current-time (floor (real-time-seconds))))
        (multiple-value-bind (prev-value present-p)
            (gethash metric-name averages (cons (list current-time 0 0) 0))
          (unless present-p
            (setf (gethash metric-name averages) prev-value))
          (let ((stats (car prev-value)))
            (if (= (first stats) current-time)
                (let ((next-n (1+ (second stats))))
                  (setf (third stats) (average (third stats) value next-n)
                        (second stats) next-n))
                (setf (cdr prev-value) (third stats)
                      (car prev-value) (list current-time 0 0)))))))))
