(in-package :cl-user)


(cl-bodge.distribution:descriptor :ball-z
  :base-directory "../"
  :entry-function (:bz main)
  :assets ("ball-z.conf"
           "run.sh"
	   "run.bat"
           "assets")
  :bundle (:name "Ball-Z" :run-file "run.sh"))
