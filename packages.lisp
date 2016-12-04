(in-package :ball-z.def)


(defpackage :ball-z
  (:use :ge.ng :ge.host :ge.gx :ge.phx :ge.snd :ge.rsc :ge.sg :ge.math :ge.util :ge.as :ge.eve
        :cl :mt)
  (:nicknames :bz)
  (:export ball-z
           main
           start))
