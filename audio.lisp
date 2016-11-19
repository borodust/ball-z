(in-package :ball-z)


(defun load-background-music (ctx)
  (let ((track (load-ogg-vorbis-audio (resource-truename "sounds/saga_musix_-_megastar.ogg"))))
    (let ((audio (ctx-audio-system ctx)))
      (-> (audio)
        (let ((source (make-audio-source audio)))
          (with-disposable ((buf (make-audio-buffer audio track)))
            (attach-audio-buffer buf source))
          (setf (ctx-background-audio ctx) source
                (audio-looped-p source) t
                (audio-gain source) 0.3)
          (play-audio source) )))))



(defun stop-background-music (ctx)
  (let ((audio (ctx-audio-system ctx)))
    (-> (audio)
      (let ((s (ctx-background-audio ctx)))
        (stop-audio s)
        (dispose s)))))
