#|
  This file is a part of simpath project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :simpath.test
  (:use :cl
        :simpath
        :fiveam
        :trivia :cl-cudd :alexandria :iterate))
(in-package :simpath.test)



(def-suite :simpath)
(in-suite :simpath)

;; run test with (run! test-name) 

(test simpath

  )



