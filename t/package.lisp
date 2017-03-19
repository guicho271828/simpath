#|
  This file is a part of mate-zdd project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :mate-zdd.test
  (:use :cl
        :mate-zdd
        :fiveam
        :trivia :cl-cudd :alexandria :iterate))
(in-package :mate-zdd.test)



(def-suite :mate-zdd)
(in-suite :mate-zdd)

;; run test with (run! test-name) 

(test mate-zdd

  )



