


(define-distribution :ball-z
    :entry-function (:bz main)
    :assets ("ball-z.conf"
             "run.sh"
             ("resources" "assets"))
    :bundle (:name "Ball-Z" :run-file "run.sh"))
