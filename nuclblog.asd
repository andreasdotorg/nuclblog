
(in-package #:cl-user)

(defpackage #:nuclblog-system (:use #:cl #:asdf))
(in-package #:nuclblog-system)

(defsystem nuclblog
  :name "nuclblog"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :depends-on (:hunchentoot :cl-who :cl-store :html-encode :net-telent-date)
  :components ((:cl-source-file "defpackage")
               (:cl-source-file "utilities":depends-on ("defpackage"))
               (:cl-source-file "nuclblog" :depends-on ("defpackage" "utilities"))
               (:cl-source-file "pages" :depends-on ("defpackage" "utilities" "nuclblog"))
               (:cl-source-file "handlers" :depends-on ("defpackage" "utilities" "nuclblog"))
               (:module "css")))
