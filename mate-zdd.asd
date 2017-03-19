#|
  This file is a part of mate-zdd project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  implementation of mate-zdd, a path enumeration algorithm

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage mate-zdd-asd
  (:use :cl :asdf))
(in-package :mate-zdd-asd)


(defsystem mate-zdd
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :bug-tracker "https://github.com/guicho271828/mate-zdd/issues"
  :source-control (:git "https://github.com/guicho271828/mate-zdd.git")
  :license "LLGPL"
  :depends-on (:trivia :cl-cudd :alexandria :iterate)
  :components ((:module "src"
                :components
                ((:file "package"))))
  :description "implementation of mate-zdd, a path enumeration algorithm"
  :in-order-to ((test-op (test-op :mate-zdd.test))))
