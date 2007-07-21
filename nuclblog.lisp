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

(defclass blog ()
  ((short-name :initarg :short-name
               :accessor blog-short-name
               :documentation "An abbreviated name for this blog.")
   (title :initarg :title
          :accessor blog-title
          :documentation "The title of this blog.")
   (subtitle :initarg :subtitle
             :accessor blog-subtitle
             :documentation "The subtitle of this blog.")
   (logo-img-url :initarg :logo-img-url
                 :accessor blog-logo-img-url
                 :initform nil
                 :documentation "The URL of the logo for this blog.")
   (owner-email :initarg :owner-email
                :accessor blog-owner-email
                :documentation "The email address of the owner of this
blog.")
   (page-css :initarg :page-css
             :accessor blog-page-css
             :initform nil
             :documentation "A URL to the CSS style to load for the
pages for this blog.")
   (categories :initarg :categories
               :accessor blog-categories
               :initform nil
               :documentation "A list of the categories of the
possible categories for entries in this blog.")
   (url-root :initarg :url-root
             :accessor blog-url-root
             :documentation "The URL for the root of this
blog. Prepended to the suffixes of various blog-related URLs")
   (buttons :initarg :buttons
            :accessor blog-buttons
            :initform nil
            :documentation "A list of lambda-lists of the form (&key
href-url id img-url alt) that specifies the the buttons to be
displayed on blog pages.")
   (entries :accessor blog-entries
            :initform nil
            :documentation "A list of the entries for this blog. This
should eventually be changed to something more flexible to allow for
other representations of blog entries.")
   (entry-storage-path :initarg :entry-storage-path
                       :accessor blog-entry-storage-path
                       :initform nil
                       :documentation "The path to the file to store
the blog entries. This should be replaced with something more
flexible.")
   (realm :initarg :realm
          :accessor blog-realm
          :initform nil
          :documentation "A realm to manage users and passwords for
  the blog.")
   (handler-alist :initarg :handler-alist
                  :accessor blog-handler-alist
                  :initform nil
                  :documentation "An alist of (URL . function) pairs
for use by the blog-dispatcher.")
   (use-ssl-p :initarg :use-ssl
              :accessor blog-use-ssl-p
              :initform nil
              :documentation "If non-nil, use https pages for
authorized blog pages.")
   (ssl-port :initarg :ssl-port
             :accessor blog-ssl-port
             :initform nil
             :documentation "If use-ssl-p is non-nil, the port on
which to use https links. If this is no port is explicitly specified
and, presumably, the browser will use 443.")
   ;; unused slots
   (authenticator :initarg :authenticator
                  :accessor blog-authenticator
		  :initform nil
                  :documentation "(Currently unused) the authenticator
object for this blog.")
   (users :initarg :users
          :accessor blog-users
          :documentation "(Currently unused) slot for the users of
this blog.")
   (blog-links :initarg :blog-links
               :accessor blog-blog-links
               :initform nil
               :documentation "(Currently unused) slot for outgoing
links for this blog."))
  (:documentation "Objects of this class represent nuclblog instances."))

(defclass blog-entry ()
  ((category :initarg :category :accessor blog-entry-category)
   (user :initarg :user :accessor blog-entry-user)
   (number :initarg :number :accessor blog-entry-number)
   (title :initarg :title :accessor blog-entry-title)
   (time :initarg :time :accessor blog-entry-time)
   (revised-time :initarg :revised-time :initform 0 :accessor blog-entry-revised-time)
   (contents :initarg :contents :accessor blog-entry-contents)
   (trackbacks :initarg :trackbacks :accessor blog-entry-trackbacks :initform nil)))

(defgeneric read-blog-entries (blog &key path))

(defmethod shared-initialize :after ((blog blog) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  ;; read passwords
  (when (blog-realm blog)
    (hunchentoot-auth:read-realm-passwords (blog-realm blog)))
  ;; read blog entries
  (read-blog-entries blog)
  ;; setup the standard blog handlers for this blog
  (define-blog-handlers blog))

(defun get-next-entry-number (blog)
  (let ((numbers (mapcar #'blog-entry-number (blog-entries blog))))
    (if numbers
        (1+ (reduce #'max numbers))
        0)))

(defparameter *entries-file-lock* (hunchentoot::make-lock "entries-file-lock"))
(defparameter *entries-lock* (hunchentoot::make-lock "entries-lock"))

(defmethod store-blog-entries (blog path)
  (ensure-directories-exist path)
  (hunchentoot::with-lock (*entries-file-lock*)
    (cl-store:store (blog-entries blog) path)))

(defmethod read-blog-entries (blog &key (path (blog-entry-storage-path blog)))
  (when (and path
             (probe-file path))
     (hunchentoot::with-lock (*entries-file-lock*)
       (setf (blog-entries blog)
             (cl-store:restore path)))))

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
    (hunchentoot::with-lock (*entries-lock*)
      (setf (blog-entries blog)
            (cons entry (blog-entries blog)))
      (let ((path (blog-entry-storage-path blog)))
        (when path (store-blog-entries blog path))))))

(defun delete-blog-entry (blog number)
  (hunchentoot::with-lock (*entries-lock*)
    (when (find number (blog-entries blog) :key #'blog-entry-number)
      (setf (blog-entries blog)
            (delete number (blog-entries blog) :key #'blog-entry-number))
      (let ((path (blog-entry-storage-path blog)))
        (when path (store-blog-entries blog path)))
      t)))

(defun get-entry (number blog)
  (hunchentoot::with-lock (*entries-lock*)
    (find number (blog-entries blog) :key #'blog-entry-number)))

(defun get-blog-entries (blog &key category)
  (hunchentoot::with-lock (*entries-lock*)
    (cond ((null category)
           (copy-seq (blog-entries blog)))
          ((atom category)
           (remove-if-not (lambda (x)
                            (equal (blog-entry-category x)
                                   category))
                          (blog-entries blog)))
          ((listp category)
           (remove-if-not (lambda (x)
                            (member (blog-entry-category x)
                                    category
                                    :test 'equal))
                          (blog-entries blog))))))

(defun sorted-blog-entries (blog &key category)
  (sort (apply #'get-blog-entries blog
               (when category
                 `(:category ,category)))
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

(defun dispatch-blog-handlers (request &optional vhost)
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

(defmethod add-user ((blog blog) user password)
  (hunchentoot-auth:add-user (blog-realm blog) user password))

