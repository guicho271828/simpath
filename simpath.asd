#|
  This file is a part of simpath project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  implementation of simpath, a path enumeration algorithm

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage simpath-asd
  (:use :cl :asdf))
(in-package :simpath-asd)


(defsystem simpath
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :bug-tracker "https://github.com/guicho271828/simpath/issues"
  :source-control (:git "https://github.com/guicho271828/simpath.git")
  :license "LLGPL"
  :depends-on (:trivia :cl-cudd :alexandria :iterate :arrow-macros)
  :components ((:module "src"
                :components
                ((:file "package"))))
  :description "implementation of simpath, a path enumeration algorithm"
  :in-order-to ((test-op (test-op :simpath.test))))
