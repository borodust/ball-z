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
  :depends-on (log4cl cl-bodge cl-fad)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "metrics")
               (:file "events")
               (:file "chain-registry")
               (:file "context")
               (:file "background-audio")
               (:file "ball-audio")
               (:file "ball-body")
               (:module nodes
                        :serial t
                        :components ((:file "hud")
                                     (:file "camera")
                                     (:file "debug")
                                     (:file "start-screen")
                                     (:file "ball-mesh")
                                     (:file "ball-model")
                                     (:file "stage")))
               (:file "scene")
               (:file "system")))


(defsystem ball-z/distrib
  :description "Ball-Z distribution"
  :version "0.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/distribution)
  :components ((:file "distribution")))
