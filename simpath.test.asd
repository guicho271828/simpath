#|
  This file is a part of simpath project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage simpath.test-asd
  (:use :cl :asdf))
(in-package :simpath.test-asd)


(defsystem simpath.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of simpath"
  :license "LLGPL"
  :depends-on (:simpath
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval
 (read-from-string
  "(let ((res (5am:run :simpath)))
     (explain! res)
     (every #'fiveam::TEST-PASSED-P res))"))
))
