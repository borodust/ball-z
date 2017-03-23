(in-package :ball-z)


(defun load-background-music (ctx)
  (let ((track (load-ogg-vorbis-audio (resource-truename "sounds/saga_musix_-_megastar.ogg"))))
    (run (-> ((audio)) ()
           #++(let ((source (make-audio-source)))
             (with-disposable ((buf (make-audio-buffer track)))
               (attach-audio-buffer buf source))

             (setf (ctx-background-audio ctx) source
                   (audio-looped-p source) t
                   (audio-gain source) 0.3)
             (play-audio source))
           (post (make-instance 'game-loaded-event) (events))))))


(defun stop-background-music (ctx)
  (run (-> ((audio)) ()
         #++(let ((s (ctx-background-audio ctx)))
           (stop-audio s)
           (dispose s)))))
