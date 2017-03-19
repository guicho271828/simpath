#|
  This file is a part of mate-zdd project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage mate-zdd.test-asd
  (:use :cl :asdf))
(in-package :mate-zdd.test-asd)


(defsystem mate-zdd.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of mate-zdd"
  :license "LLGPL"
  :depends-on (:mate-zdd
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval
 (read-from-string
  "(let ((res (5am:run :mate-zdd)))
     (explain! res)
     (every #'fiveam::TEST-PASSED-P res))"))
))
