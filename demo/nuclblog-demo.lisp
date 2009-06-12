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

;;;
;;; for debugging hunchentoot errors
#+nil
(progn
  (setf hunchentoot::*show-lisp-backtraces-p* t)
  (setf hunchentoot::*show-lisp-errors-p* t))

(defparameter *blog*
  (make-instance 'blog:blog
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
                                      (ch-asdf:asdf-lookup-path "asdf:/nuclblog-demo-data/demo/storage"))
                 :realm
                 (make-instance 'hunchentoot-auth:realm
                                :user-storage-path (merge-pathnames
                                                    "user.store"
                                                    (ch-asdf:asdf-lookup-path "asdf:/nuclblog-demo-data/demo/storage"))
                                :group-storage-path (merge-pathnames
                                                     "group.store"
                                                     (ch-asdf:asdf-lookup-path "asdf:/nuclblog-demo-data/demo/storage")))
                 :buttons '((:href-url "http://weitz.de/hunchentoot/"
                             :id "hunchentoot-button"
                             :img-url "/static/hunchentoot10.png"
                             :alt "hunchentoot")
                            (:href-url "http://www.sbcl.org/"
                             :id "sbclbutton"
                             :img-url "/static/sbclbutton.png"
                             :alt "(get 'sbcl)"))))
    
  

(defparameter *localhost-host*
  (hunchentoot-vhost:make-virtual-host "localhost"
                                       '("localhost"
                                         "127.0.0.1")))

(defun initialize-blog (blog host)
  (pushnew (lambda (request)
             (nuclblog::blog-dispatch request blog))
           (hunchentoot-vhost::virtual-host-dispatch-table host) :test #'equal))

(defun initialize-server (server)

  ;; add the virtual host to the server
  (hunchentoot-vhost::add-virtual-host *localhost-host* server)

  ;; initialize the blog (that is, associate the blog with the virtual
  ;; host and add a function that will (with the appropriate args)
  ;; call blog-dispatch
  (initialize-blog *blog* *localhost-host*)
    
  (pushnew (hunchentoot::create-folder-dispatcher-and-handler
            "/nuclblog-css/"
            (ch-asdf:asdf-lookup-path "asdf:/nuclblog/css"))
           (hunchentoot-vhost::virtual-host-dispatch-table *localhost-host*) :test #'equal)

  (pushnew (hunchentoot::create-folder-dispatcher-and-handler
            "/static/"
            (ch-asdf:asdf-lookup-path "asdf:/nuclblog-demo/demo/static"))
           (hunchentoot-vhost::virtual-host-dispatch-table *localhost-host*) :test #'equal))


(defun start-ssl-services (blog &key (port 4243))
  (let ((key-file (ch-asdf:asdf-lookup-path "asdf:/nuclblog-demo-data/demo/ssl/key-pem"))
        (cert-file (ch-asdf:asdf-lookup-path "asdf:/nuclblog-demo-data/demo/ssl/certificate-pem")))
    (print (list port key-file cert-file))
    (let ((ssl-acceptor (make-instance 'hunchentoot:ssl-acceptor
                                     :ssl-privatekey-file key-file
                                     :ssl-certificate-file cert-file
                                     :port port)))
      (hunchentoot:start ssl-acceptor)
      (initialize-server ssl-acceptor)
      (setf (blog::blog-ssl-port blog) port)
      ssl-acceptor)))

(defun start-services (&key
                       (port 4242)
                       (use-ssl t)
                       ssl-port)
  (let ((access-log-path (ch-asdf:asdf-lookup-path
                   "asdf:/nuclblog-demo-data/demo/log/nuclblog-demo-access-log")))
    (ensure-directories-exist access-log-path)
    (setf hunchentoot:*access-log-pathname*
          access-log-path))
  (let ((message-log-path (ch-asdf:asdf-lookup-path
                          "asdf:/nuclblog-demo-data/demo/log/nuclblog-demo-message-log")))
    (ensure-directories-exist message-log-path)
    (setf hunchentoot:*message-log-pathname*
          message-log-path))
  
  (let ((blog *blog*))
    (let ((acceptor (make-instance 'hunchentoot:acceptor :port port)))
      (hunchentoot:start acceptor)
      (initialize-server acceptor)
      (if use-ssl
          (progn
            (setf (blog::blog-use-ssl-p blog) t)
            (let ((ssl-acceptor (apply #'start-ssl-services blog
                                     (when ssl-port
                                       `(:port ,ssl-port)))))
              (values acceptor ssl-acceptor)))
          acceptor))))

