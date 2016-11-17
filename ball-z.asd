(in-package :cl-user)

(defpackage :ball-z.def
  (:use :cl :asdf))

(in-package :ball-z.def)


(defsystem ball-z
  :description "Game with balls"
  :version "0.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (log4cl cl-bodge cl-fad trivial-garbage)
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "events")
               (:file "camera")
               (:file "chain-registry")
               (:file "context")
               (:module models
                        :serial t
                        :components ((:file "simple-mesh")
                                     (:file "ball")
                                     (:file "stage")))
               (:file "scene")
               (:file "system")))
