;;; file: authorization.lisp
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

(in-package :nuclblog)

(defparameter *password-file-lock* (make-lock "password-file-lock"))
(defparameter *password-lock* (make-lock "password-lock"))

(defun read-blog-passwords (blog &key (path (blog-password-storage-path blog)))
  (when (probe-file path)
    (with-lock (*password-file-lock*)
      (setf (blog-passwords blog)
            (cl-store:restore path)))))

(defun store-blog-passwords (blog &key (path (blog-password-storage-path blog)))
  (ensure-directories-exist path)
  (with-lock (*password-file-lock*)
    (cl-store:store (blog-passwords blog) path)))

(defmethod get-password-hash ((blog blog) user)
  (with-lock (*password-lock*)
    (gethash user (blog-passwords blog))))

(defmethod set-password ((blog blog) user password)
  (with-lock (*password-lock*)
    (setf (gethash user (blog-passwords blog))
          (md5:md5sum-sequence (coerce password 'simple-string)))
    (store-blog-passwords blog)))

(defmethod add-user ((blog blog) user password)
  (set-password blog user password))

(defmethod check-password ((blog blog) user password)
  (and password
       (equalp (get-password-hash blog user)
               (md5:md5sum-sequence (coerce password 'simple-string)))))

(defmacro authorized-page ((user password) &rest body)
  `(if (or (not (blog-use-ssl-p blog))
           (ssl-p))
       (if (or (and ,user ,password
                    (check-password blog ,user ,password))
               (session-value 'user-authenticated-p))
           (progn
             (unless (session-value 'user-authenticated-p)
               (setf (session-value 'user) user)
               (setf (session-value 'user-authenticated-p) t))
             ,@body)
           (blog::blog-page
            blog
            (format nil "~A: login" (blog-title blog))
            (lambda ()
              (with-html
                "Please login:"
                (:form :method :post
                       "Name: "
                       (if ,user
                           (htm (:input :type :text :name "user" :value ,user))
                           (htm (:input :type :text :name "user")))
                       (:br)
                       "Password: "
                       (if ,password
                           (htm (:input :type :password :name "password" :value ,password))
                           (htm (:input :type :password :name "password")))
                       (:br)
                       (:input :type :submit :value "Submit"))))))
       (apply #'redirect (request-uri)
              :protocol :https
              (let ((port (blog-ssl-port blog)))
                (when port
                  `(:port ,port))))))
