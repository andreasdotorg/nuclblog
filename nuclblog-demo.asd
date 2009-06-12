;;; file: nuclblog-demo.asd
;;;
;;; Copyright (c) 2007 Cyrus Harmon (ch-lisp@bobobeach.com)
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(asdf:defsystem nuclblog-demo
  :name "nuclblog-demo"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :depends-on (:hunchentoot :cl-who :ch-asdf :nuclblog :hunchentoot-vhost)
  :components
  ((:module
    :demo
    :components
    ((:static-file "README")
     (:cl-source-file "defpackage")
     (:cl-source-file "nuclblog-demo" :depends-on ("defpackage"))
     (:module "static"
      :components ((:static-file demoblog-css :pathname #p"demoblog.css")
                   (:static-file black-css :pathname #p"black.css")
                   (:static-file white-css :pathname #p"white.css")
                   (:static-file sbclbutton-png :pathname #p"sbclbutton.png")))
     (:module "ssl"
      :components ((:static-file "openssl-config"
                    :pathname #p"openssl.config")))))))

(asdf:defsystem nuclblog-demo-data
  :name "nuclblog-demo-data"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :depends-on (:nuclblog-demo)
  :components
  ((:module
    :demo
    :components
    ((:module "log"
      :components ((:static-file "nuclblog-demo-access-log"
                                 :pathname #p"nuclblog-demo-access-log")
                   (:static-file "nuclblog-demo-message-log"
                                 :pathname #p"nuclblog-demo-message-log")))
     (:module "ssl"
      :components ((:static-file "key-pem"
                    :pathname #p"key.pem")
                   (:static-file "certificate-pem"
                    :pathname #p"certificate.pem")))
     (:module "storage")))))
