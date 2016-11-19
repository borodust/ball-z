(in-package :ball-z)


(defclass shared-audio (shared-resource) ())


(defmethod dispose-resource ((this shared-audio) resource)
  (dispose resource))


(defclass ball-audio (disposable)
  ((shared-pop :initform nil :allocation :class)
   (shared-strike :initform nil :allocation :class)
   (shared-fail :initform nil :allocation :class)

   (pop :initform nil)
   (strike :initform nil)
   (fail :initform nil)))


(define-destructor ball-audio (shared-pop shared-strike shared-fail)
  (release-resource shared-pop)
  (release-resource shared-strike)
  (release-resource shared-fail))


(defun %make-shared-audio-source (audio path)
  (let* ((track (load-ogg-vorbis-audio (resource-truename path)))
         (source (make-audio-source audio)))
    (with-disposable ((buf (make-audio-buffer audio track)))
      (attach-audio-buffer buf source))
    (make-instance 'shared-audio :resource source)))


(defmethod initialize-instance :after ((this ball-audio) &key audio)
  (with-slots (shared-pop shared-strike shared-fail pop strike fail) this
    (when (or (null shared-pop) (resource-disposed-p shared-pop))
      (setf shared-pop (%make-shared-audio-source audio "sounds/ball-pop.ogg")
            shared-strike (%make-shared-audio-source audio "sounds/ball-explosion.ogg")
            shared-fail (%make-shared-audio-source audio "sounds/strike-fail.ogg")))
    (setf pop (acquire-resource shared-pop)
          strike (acquire-resource shared-strike)
          fail (acquire-resource shared-fail))))


(defun play-pop-sound (ball-audio)
  (with-slots (pop) ball-audio
    (play-audio pop)))


(defun play-strike-sound (ball-audio)
  (with-slots (strike) ball-audio
    (play-audio strike)))


(defun play-fail-sound (ball-audio)
  (with-slots (fail) ball-audio
    (play-audio fail)))
