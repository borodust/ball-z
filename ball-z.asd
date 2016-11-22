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
  :depends-on (log4cl cl-bodge cl-fad trivial-garbage bordeaux-threads
                      texatl-client cl-conspack local-time)
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "events")
               (:file "text")
               (:file "start-screen")
               (:file "camera")
               (:file "chain-registry")
               (:file "context")
               (:file "audio")
               (:module models
                        :serial t
                        :components ((:file "ball-mesh")
                                     (:file "ball-audio")
                                     (:file "ball-body")
                                     (:file "ball")
                                     (:file "stage")))
               (:file "scene")
               (:file "system")))
