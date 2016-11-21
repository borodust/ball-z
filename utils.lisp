(in-package :ball-z)


(define-constant +half-pi+ (/ pi 2))


(defvar *configuration-pathname* nil)


(defun resource-truename (relative-path)
  (fad:merge-pathnames-as-file
   *configuration-pathname*
   (property :resources "resources/")
   relative-path))


(defgeneric ball-type-of (ball))


;;;
;;;
;;;
(defclass shared-resource ()
  ((resource :initform (error "resource must be provided")
             :initarg :resource :reader %resource-of)
   (count :initform 0)))


(defgeneric dispose-resource (resource obj))


(defgeneric acquire-resource (resource)
  (:method ((resource shared-resource))
    (with-slots (count resource) resource
      (incf count)
      resource)))


(defgeneric release-resource (resource)
  (:method ((this shared-resource))
    (with-slots (count resource) this
      (decf count)
      (when (= count 0)
        (unwind-protect
             (dispose-resource this resource)
          (setf resource nil))))))


(defgeneric resource-disposed-p (resource)
  (:method ((this shared-resource))
    (with-slots (resource) this
      (null resource))))
