;;; file: nuclblog-demo.lisp
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

(in-package :nuclblog-demo)

;;; for debugging

#+nil
(progn
  (setf hunchentoot::*show-lisp-backtraces-p* t)
  (setf hunchentoot::*show-lisp-errors-p* t))

(defclass nuclblog-demo-blog (blog:blog)
  ())

(defparameter *blog*
  (make-instance 'nuclblog-demo-blog
                 :short-name "nuclblog demo"
                 :title "a demo blog for nuclblog"
                 :subtitle "witness the awesome power of this fully operational hunchentoot!"
                 :logo-img-url "/images/blog-logo.png"
                 :owner-email "your_name_here@localhost"
                 :page-css (list (cons "demoblog" "/static/demoblog.css"))
                 :categories (list "Lisp"
                                   "Music"
                                   "Food"
                                   "Wine"
                                   "General")
                 :url-root "/blog"
                 :entry-storage-path (merge-pathnames
                                      "entries.store"
                                      (ch-asdf:asdf-lookup-path "asdf:/nuclblog-demo/demo/storage"))

                 :realm
                 (make-instance 'hunchentoot-auth:realm
                                :password-storage-path (merge-pathnames
                                                        "passwd.store"
                                                        (ch-asdf:asdf-lookup-path "asdf:/nuclblog-demo/demo/storage")))
                 :buttons '((:href-url "http://weitz.de/hunchentoot/"
                             :id "hunchentoot-button"
                             :img-url "/static/hunchentoot10.png"
                             :alt "hunchentoot")
                            (:href-url "http://www.sbcl.org/"
                             :id "sbclbutton"
                             :img-url "/static/sbclbutton.png"
                             :alt "(get 'sbcl)"))
                 :use-ssl t
                 :ssl-port 4243))

#-no-hunchentoot-vhost
(progn

  (defparameter *localhost-host*
    (hunchentoot-vhost:make-virtual-host "localhost" '("localhost")))

  (pushnew 'hunchentoot-vhost:dispatch-virtual-host-handlers
           hunchentoot:*dispatch-table* :test #'equal)

  (pushnew 'nuclblog::dispatch-blog-handlers
           (hunchentoot-vhost::dispatch-table *localhost-host*) :test #'equal)

  (pushnew (hunchentoot-vhost::create-virtual-host-folder-dispatcher-and-handler
            "/nuclblog-css/"
            (ch-asdf:asdf-lookup-path "asdf:/nuclblog/css"))
           (hunchentoot-vhost::dispatch-table *localhost-host*) :test #'equal)

  (pushnew (hunchentoot-vhost::create-virtual-host-folder-dispatcher-and-handler
            "/static/"
            (ch-asdf:asdf-lookup-path "asdf:/nuclblog-demo/demo/static"))
           (hunchentoot-vhost::dispatch-table *localhost-host*) :test #'equal))

#+no-hunchentoot-vhost
(setf *dispatch-table*
      (list #'blog::dispatch-blog-handlers
            #'dispatch-easy-handlers
            (create-folder-dispatcher-and-handler
             "/nuclblog-css/"
             (ch-asdf:asdf-lookup-path "asdf:/nuclblog/css"))
            (create-folder-dispatcher-and-handler
             "/static/"
             (ch-asdf:asdf-lookup-path "asdf:/nuclblog-demo/demo/static"))
            #'default-dispatcher))

(defun start-services (&key
                       (ssl nil)
                       (port (if ssl
                                 (let ((blog-ssl-port (blog::blog-ssl-port *blog*)))
                                   (if blog-ssl-port
                                       blog-ssl-port
                                       4243)) 
                                 4242)))
  (setf (hunchentoot:log-file)
        (ch-asdf:asdf-lookup-path "asdf:/nuclblog-demo/demo/log/nuclblog-demo-log"))
  (if ssl
      (let ((key-file (ch-asdf:asdf-lookup-path "asdf:/nuclblog-demo/demo/ssl/key-pem"))
            (cert-file (ch-asdf:asdf-lookup-path "asdf:/nuclblog-demo/demo/ssl/certificate-pem")))
        (print (cons key-file cert-file))
        (hunchentoot:start-server
               :ssl-privatekey-file key-file
               :ssl-certificate-file cert-file
               :port port))
      (hunchentoot:start-server :port port)))

(defun start-ssl-services (&key (port (blog::blog-ssl-port *blog*)))
  (setf (blog::blog-use-ssl-p *blog*) t)
  (setf (blog::blog-ssl-port *blog*) port)
  (start-services :port port :ssl t))
