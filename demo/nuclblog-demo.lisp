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

(defparameter *blog* (make-instance 'blog:blog
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
                                                   :user-storage-path (merge-pathnames
                                                                       "user.store"
                                                                       (ch-asdf:asdf-lookup-path "asdf:/nuclblog-demo/demo/storage"))
                                                   :group-storage-path (merge-pathnames
                                                                        "group.store"
                                                                        (ch-asdf:asdf-lookup-path "asdf:/nuclblog-demo/demo/storage")))
                                    :buttons '((:href-url "http://weitz.de/hunchentoot/"
                                                :id "hunchentoot-button"
                                                :img-url "/static/hunchentoot10.png"
                                                :alt "hunchentoot")
                                               (:href-url "http://www.sbcl.org/"
                                                :id "sbclbutton"
                                                :img-url "/static/sbclbutton.png"
                                                :alt "(get 'sbcl)"))))
    
  

#-no-hunchentoot-vhost
(defparameter *localhost-host*
  (hunchentoot-vhost:make-virtual-host "localhost"
                                       '("localhost")))

(defun initialize-blog (blog host)
  (pushnew (lambda (request &optional vhost)
             (declare (ignore vhost))
             (nuclblog::blog-dispatch request blog))
           (hunchentoot-vhost::dispatch-table host) :test #'equal))

#-no-hunchentoot-vhost
(defun initialize-server (server)

  ;; add the virtual host to the server
  (hunchentoot-vhost::add-virtual-host *localhost-host* server)

  ;; initialize the blog (that is, associate the blog with the virtual
  ;; host and add a function that will (with the appropriate args)
  ;; call blog-dispatch
  (initialize-blog *blog* *localhost-host*)
    
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

(defun start-ssl-services (blog &key (port 4243))
  (setf (ht-auth::realm-ssl-port (blog::blog-realm blog)) port)
  (let ((key-file (ch-asdf:asdf-lookup-path "asdf:/nuclblog-demo/demo/ssl/key-pem"))
        (cert-file (ch-asdf:asdf-lookup-path "asdf:/nuclblog-demo/demo/ssl/certificate-pem")))
    (print (cons key-file cert-file))
    (let ((ssl-server (hunchentoot:start-server
                       :ssl-privatekey-file key-file
                       :ssl-certificate-file cert-file
                       :port port)))
      (initialize-server ssl-server)
      (setf (blog::blog-ssl-port blog) port)
      ssl-server)))

(defun start-services (&key
                       (port 4242)
                       (use-ssl t)
                       (ssl-port 4243))
  (setf (hunchentoot:log-file)
        (ch-asdf:asdf-lookup-path "asdf:/nuclblog-demo/demo/log/nuclblog-demo-log"))
  (let ((blog *blog*))
    (let ((server (hunchentoot:start-server :port port)))
      (initialize-server server)
      (if use-ssl
          (progn
            (setf (blog::blog-use-ssl-p blog) t)
            (let ((ssl-server (start-ssl-services blog)))
              (values server ssl-server)))
          server))))

