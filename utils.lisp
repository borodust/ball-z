(in-package :ball-z)


(define-constant +half-pi+ (/ pi 2))


(defvar *configuration-pathname* nil)


(defun resource-truename (relative-path)
  (fad:merge-pathnames-as-file
   *configuration-pathname*
   (property :resources "resources/")
   relative-path))


(defgeneric ball-type-of (ball))


(defclass shared-resource ()
  ((count :initform 0)))


(defgeneric dispose-resource (resource))


(defgeneric acquire-resource (resource)
  (:method ((resource shared-resource))
    (with-slots (count) resource
      (incf count))))


(defgeneric release-resource (resource)
  (:method ((resource shared-resource))
    (with-slots (count) resource
      (decf count)
      (when (= count 0)
        (dispose-resource resource)))))
