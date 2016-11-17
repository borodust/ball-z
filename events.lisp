(in-package :ball-z)


(defevent ball-collision-event (event)
  (this-ball that-ball))


(defevent chain-broke-event (event)
  (balls))
