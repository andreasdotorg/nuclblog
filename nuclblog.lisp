;;; file: nuclblog.lisp
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

(defclass authenticator () ())

(defgeneric auth-get-password (authenticator user))

(defclass blog ()
  ((short-name :initarg :short-name :accessor blog-short-name)
   (title :initarg :title :accessor blog-title)
   (subtitle :initarg :subtitle :accessor blog-subtitle)
   (logo-img-url :initarg :logo-img-url :accessor blog-logo-img-url :initform nil)
   (users :initarg :users :accessor blog-users)
   (owner-email :initarg :owner-email :accessor blog-owner-email)
   (blog-links :initarg :blog-links :accessor blog-blog-links :initform nil)
   (people-links :initarg :people-links :accessor blog-people-links :initform nil)
   (page-css :initarg :page-css :accessor blog-page-css :initform nil)
   (categories :initarg :categories :accessor blog-categories :initform nil)
   (authenticator :initarg :authenticator :accessor blog-authenticator
		  :initform nil)
   (allow-html-editor :initarg allow-html-editor :initform nil
		      :accessor blog-allow-html-editor)
   (url-root :initarg :url-root :accessor blog-url-root)
   (buttons :initarg :buttons :accessor blog-buttons :initform (list))
   (entries :accessor blog-entries :initform (list))
   (entry-storage-path :initarg :entry-storage-path
                       :accessor blog-entry-storage-path :initform nil)
   (passwords :accessor blog-passwords :initform (make-hash-table :test 'equal))
   (password-storage-path :initarg :password-storage-path
                          :accessor blog-password-storage-path :initform nil)
   (handler-alist :initarg :handler-alist :accessor blog-handler-alist :initform (list))))

(defclass blog-entry ()
  ((category :initarg :category :accessor blog-entry-category)
   (user :initarg :user :accessor blog-entry-user)
   (number :initarg :number :accessor blog-entry-number)
   (title :initarg :title :accessor blog-entry-title)
   (time :initarg :time :accessor blog-entry-time)
   (revised-time :initarg :revised-time :initform 0 :accessor blog-entry-revised-time)
   (contents :initarg :contents :accessor blog-entry-contents)
   (trackbacks :initarg :trackbacks :accessor blog-entry-trackbacks :initform nil)))

(defun get-next-entry-number (blog)
  (let ((numbers (mapcar #'blog-entry-number (blog-entries blog))))
    (if numbers
        (1+ (reduce #'max numbers))
        0)))

(defun store-blog-entries (blog path)
  (ensure-directories-exist path)
  (cl-store:store (blog-entries blog) path))

(defun read-blog-entries (blog &key (path (blog-entry-storage-path blog)))
  (when (probe-file path)
    (setf (blog-entries blog)
          (cl-store:restore path))))

(defun create-blog-entry (blog category title contents user
                          &key
                          (number (get-next-entry-number blog))
                          (time (get-universal-time)))
  (let ((entry (make-instance 'blog-entry
                              :category category
                              :user user
                              :number number
                              :title title
                              :time time
                              :revised-time time
                              :contents contents)))
    (setf (blog-entries blog)
          (cons entry (blog-entries blog))))
  (let ((path (blog-entry-storage-path blog)))
    (when path (store-blog-entries blog path))))

(defun delete-blog-entry (blog number)
  (when (find number (blog::blog-entries blog) :key #'blog::blog-entry-number)
    (setf (blog::blog-entries blog)
          (delete number (blog::blog-entries blog) :key #'blog::blog-entry-number))
    (let ((path (blog-entry-storage-path blog)))
      (when path (store-blog-entries blog path)))
    t))

(defun get-entry (number blog)
  (find number (blog-entries blog) :key #'blog-entry-number))

(defun sorted-blog-entries (blog)
  (sort (copy-seq (blog-entries blog))
        #'>
        :key #'blog-entry-time))

;;; Originally we just used an easy-handler. The problem with the
;;; easy-handler is that it defuns a function of the given name, so we
;;; could, for instance, only have one blog-new function defined at a
;;; time. Clearly, there are other ways to do this, but we define our
;;; own dispatch table, ripping off the easy dispatch table code along
;;; the way, but pushing the closure directly into the alist, rather
;;; than the name of the function. In addition, we move the call to
;;; blog-url-root inside the define-blog-handler so that the :uri
;;; passed in will be made relative to the blog root.
(defparameter *blog-dispatch-blogs* nil
  "")

(defun dispatch-blog-handlers (request)
  "The dispatch function for the blog handlers. This should be added
to the hunchentoot:*dispatch-table*."
  (loop for blog in *blog-dispatch-blogs*
     do
     (loop for (uri . handler) in (blog-handler-alist blog)
        when (cond ((stringp uri)
                    (string= (script-name request) uri))
                   (t (funcall uri request)))
        do
        (return-from dispatch-blog-handlers handler))))

(defmacro define-blog-handler (description lambda-list &body body)
  "Like define-easy-handler, except it takes a first argument
blog. If description is an atom, it is the blog for which the
handler is to be defined. If it is a list, the first item is the
blog, followed by the keyword args. The keyword :uri argument in
the description will be appended to the blog-url-root of the
specified blog. See define-easy-handler for further details."
  (when (atom description)
    (setq description (list description)))
  (destructuring-bind (blog
                       &key
                       uri
                       (default-parameter-type ''string)
                       (default-request-type :both))
      description
    (hunchentoot::with-unique-names (cons uri%)
      `(progn
         (pushnew ,blog *blog-dispatch-blogs*)
         (let ((,uri% (concatenate-url (blog-url-root ,blog) ,uri)))
           (setf (blog-handler-alist ,blog)
                 (delete-if (lambda (,cons)
                              (equal ,uri% (car ,cons)))
                            (blog-handler-alist ,blog)))
           (push (cons ,uri%
                       (lambda (&key ,@(loop for part in lambda-list
                                          collect (hunchentoot::make-defun-parameter
                                                   part
                                                   default-parameter-type
                                                   default-request-type)))
                         ,@body))
                 (blog-handler-alist ,blog)))))))





