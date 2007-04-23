
(in-package #:cl-user)

(defpackage #:nuclblog-system (:use #:cl #:asdf))
(in-package #:nuclblog-system)

(defsystem nuclblog
  :name "nuclblog"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :licence "BSD"
  :depends-on (:hunchentoot :cl-who :cl-store :puri)
  :components
  ((:static-file "README")
   (:static-file "LICENSE")
   (:cl-source-file "defpackage")
   (:cl-source-file "utilities":depends-on ("defpackage"))
   (:cl-source-file "nuclblog" :depends-on ("defpackage" "utilities"))
   (:cl-source-file "links" :depends-on ("defpackage" "utilities" "nuclblog"))
   (:cl-source-file "pages" :depends-on ("defpackage"
                                         "utilities"
                                         "nuclblog"
                                         "links"))
   (:cl-source-file "handlers" :depends-on ("defpackage"
                                            "utilities"
                                            "nuclblog"
                                            "links"))
   (:module "css"
            :components ((:static-file nuclblog-css :pathname #p"nuclblog.css")))))
