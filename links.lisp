;;; file: links.lisp
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

(defun get-protocol ()
  (if (ssl-p) "https" "http"))

(defun make-full-root-url (blog)
  (concatenate-url (get-protocol) "://" (host) "" (blog-url-root blog)))

(defun make-full-entry-url (blog entry)
  (concatenate-url (get-protocol) "://" (host) "" (blog-url-root blog)
                   "/display?id="
                     (url-encode
                      (princ-to-string (blog-entry-number entry)))))

(defun make-entry-url (blog entry)
  (concatenate-url (blog-url-root blog)
                   "/display?id="
                   (url-encode
                    (princ-to-string (blog-entry-number entry)))))

(defun make-archives-url (blog category &key rss)
  (if rss
      (format nil "~A/archives.rss?category=~A" (blog-url-root blog)
              (url-encode category))
      (format nil "~A/archives?category=~A" (blog-url-root blog)
              (url-encode category))))

(defmethod blog-new-entry-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/new"))

(defun parse-host-name-and-port (host-and-port)
  (let ((strings
         (nth-value 1
                    (cl-ppcre:scan-to-strings "^([^:]*)(:([^:]*))?$"
                                              host-and-port))))
    (values (elt strings 0)
            (elt strings 2))))

(defmethod blog-login-url ((blog blog))
  (if (blog-use-ssl-p blog)
      (multiple-value-bind (host)
          (parse-host-name-and-port (host))
        (format nil "https://~A~@[:~A~]~A/login"
                host
                (blog-ssl-port blog)
                (blog-url-root blog)))
      (concatenate-url (blog-url-root blog) "/login")))

(defmethod blog-logout-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/logout"))

(defmethod blog-display-entry-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/display"))

(defmethod blog-archives-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/archives"))

(defmethod blog-rss-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/archives.rss"))

(defmethod blog-edit-entry-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/edit"))

(defun make-edit-entry-url (blog entry)
  (concatenate-url (blog-edit-entry-url blog)
                   "?id="
                   (url-encode
                    (princ-to-string (blog-entry-number entry)))))

(defmethod blog-delete-entry-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/delete"))

(defun make-delete-entry-url (blog entry)
  (concatenate-url (blog-delete-entry-url blog)
                   "?id="
                   (url-encode
                    (princ-to-string (blog-entry-number entry)))))

(defmethod blog-email-redirect-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/email"))

(defmethod blog-trackback-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/trackback"))

(defmethod blog-new-category-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/new-category"))

(defmethod blog-css-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/css"))

