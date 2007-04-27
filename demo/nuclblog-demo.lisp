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

#+nil (setf hunchentoot::*show-lisp-backtraces-p* t)
#+nil (setf hunchentoot::*show-lisp-errors-p* t)

(defclass nuclblog-demo-blog (blog:blog)
  ())

(defmethod shared-initialize :after ((blog nuclblog-demo-blog) slot-names &rest initargs)
  (declare (ignore initargs))
  (blog::read-blog-entries blog)
  (blog::read-blog-passwords blog)
  
  ;; setup the standard blog handlers for this blog
  (blog:define-blog-handlers blog))

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
                 :password-storage-path (merge-pathnames
                                         "passwd.store"
                                         (ch-asdf:asdf-lookup-path "asdf:/nuclblog-demo/demo/storage"))
                 :buttons '((:href-url "http://weitz.de/hunchentoot/"
                             :id "hunchentoot-button"
                             :img-url "/static/hunchentoot10.png"
                             :alt "hunchentoot")
                            (:href-url "http://www.sbcl.org/"
                             :id "sbclbutton"
                             :img-url "/static/sbclbutton.png"
                             :alt "(get 'sbcl)"))))

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

(defun start-services (&key (port 4242) (ssl nil))
  (setf (hunchentoot:log-file)
        (ch-asdf:asdf-lookup-path "asdf:/nuclblog-demo/demo/log/nuclblog-demo-log"))
  (if ssl
      (let ((key-file (ch-asdf:asdf-lookup-path "asdf:/nuclblog-demo/demo/ssl/key-pem"))
            (cert-file (ch-asdf:asdf-lookup-path "asdf:/nuclblog-demo/demo/ssl/certificate-pem")))
        (print (cons key-file cert-file))
        (hunchentoot:start-server :port port
                                  :ssl-privatekey-file key-file
                                  :ssl-certificate-file cert-file))
      (hunchentoot:start-server :port port)))

