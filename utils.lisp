(in-package :ball-z)


(define-constant +half-pi+ (/ pi 2))


(defvar *configuration-pathname* nil)


(defun resource-truename (relative-path)
  (fad:merge-pathnames-as-file
   *configuration-pathname*
   (property :resources "resources/")
   relative-path))
