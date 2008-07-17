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

(defclass blog ()
  ((short-name :initarg :short-name
               :accessor blog-short-name
               :documentation "An abbreviated name for this blog.")
   (title :initarg :title
          :initform nil
          :accessor blog-title
          :documentation "The title of this blog.")
   (subtitle :initarg :subtitle
             :accessor blog-subtitle
             :initform nil
             :documentation "The subtitle of this blog.")
   (banner :initarg :banner
           :accessor blog-banner
           :initform nil
           :documentation "A function to be used as the banner (or NIL).")
   (footer :initarg :footer
           :accessor blog-footer
           :initform nil
           :documentation "A function to be used as the footer (or NIL).")
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
  ;; read blog entries
  (read-blog-entries blog)
  ;; setup the standard blog handlers for this blog
  (define-blog-handlers blog))

(defun get-next-entry-number (blog)
  (let ((numbers (mapcar #'blog-entry-number (blog-entries blog))))
    (if numbers
        (1+ (reduce #'max numbers))
        0)))

(defparameter *entries-file-lock* (bt:make-lock "entries-file-lock"))
(defparameter *entries-lock* (bt:make-lock "entries-lock"))

(defmethod store-blog-entries (blog path)
  (ensure-directories-exist path)
  (bt:with-lock-held (*entries-file-lock*)
    (cl-store:store (blog-entries blog) path)))

(defmethod read-blog-entries (blog &key (path (blog-entry-storage-path blog)))
  (when (and path
             (probe-file path))
     (bt:with-lock-held (*entries-file-lock*)
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
    (bt::with-lock-held (*entries-lock*)
      (setf (blog-entries blog)
            (cons entry (blog-entries blog)))
      (let ((path (blog-entry-storage-path blog)))
        (when path (store-blog-entries blog path))))))

(defun delete-blog-entry (blog number)
  (bt:with-lock-held (*entries-lock*)
    (when (find number (blog-entries blog) :key #'blog-entry-number)
      (setf (blog-entries blog)
            (delete number (blog-entries blog) :key #'blog-entry-number))
      (let ((path (blog-entry-storage-path blog)))
        (when path (store-blog-entries blog path)))
      t)))

(defun get-entry (number blog)
  (bt:with-lock-held (*entries-lock*)
    (find number (blog-entries blog) :key #'blog-entry-number)))

(defun get-blog-entries (blog &key category)
  (bt:with-lock-held (*entries-lock*)
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

(defclass blog-uri-handler ()
  ((uri :initarg :uri :accessor blog-uri-handler-uri)
   (handler :initarg :handler :accessor blog-uri-handler-handler)
   (require-authorization :initarg :require-authorization 
                          :accessor blog-uri-handler-require-authorization
                          :initform nil)))

(defgeneric blog-dispatch (request blog)
  (:documentation "The dispatch (generic) function for the blog
handlers. This should be added to the hunchentoot:*dispatch-table*.")
  (:method (request blog)
    (loop for uri-handler in (blog-handler-alist blog)
       do (let ((uri (blog-uri-handler-uri uri-handler))
                (handler (blog-uri-handler-handler uri-handler)))
            (when (cond ((stringp uri)
                         (string= (script-name request) uri))
                        (t (funcall uri request)))
              (return-from blog-dispatch handler))))))

(defclass authorized-blog (blog)
  ())

(defparameter *login-page-function* 'ht-auth::login-page)

(defmethod blog-dispatch (request (blog authorized-blog))
  (declare (optimize (debug 3)))
  (let ((handler (call-next-method)))
    (when (and handler (blog-realm blog)) 
      (let ((realm (blog-realm blog))
            (user (tbnl:parameter "user"))
            (password (tbnl:parameter "password")))
        (if (or (not (blog-use-ssl-p blog)) 
                (ssl-p))
            (cond ((ht-auth:session-realm-user-authenticated-p realm)
                   handler)
                  ((and user password (ht-auth::check-password realm user password))
                   (setf (ht-auth:session-realm-user realm) user)
                   (setf (ht-auth:session-realm-user-authenticated-p realm) t)
                   handler)
                  (t *login-page-function*))
            (progn
              (apply #'redirect (request-uri request)
                     :protocol :https
                     (when (blog-ssl-port blog) 
                       (multiple-value-bind (host-name)
                           (parse-host-name-and-port (host request))
                         `(:host ,host-name :port ,(blog-ssl-port blog)))))))))))

(defmacro define-blog-handler (description lambda-list blog-fn)
  "Like define-easy-handler, except it takes a first argument
blog. If description is an atom, it is the blog for which the
handler is to be defined. If it is a list, the first item is the
blog, followed by the keyword args. The keyword :uri argument in
the description will be appended to the blog-url-root of the
specified blog. See define-easy-handler for further details.

A function designator to be funcalled is provided. This function is
then called with the blog as the first argument and keyword arguments
for each of the parameters in lambda-list. Note that the called
function need not specify default values for the keyword parameters as
those are established here."
  (when (atom description)
    (setq description (list description)))
  (destructuring-bind (blog
                       &key
                       uri
                       (default-parameter-type ''string)
                       (default-request-type :both))
      description
    (let* ((key-arg-list (loop for part in lambda-list
                            collect (hunchentoot::make-defun-parameter
                                     part
                                     default-parameter-type
                                     default-request-type)))
           (key-args (mapcar (lambda (arg)
                               (if (listp arg)
                                   (car arg)
                                   arg))
                             key-arg-list))
           (key-keywords (mapcar (lambda (arg)
                                   (make-keyword
                                    (symbol-name arg)))
                                 key-args)))
      (with-unique-names (uri-handler uri%)
        `(progn
           (pushnew ,blog *blog-dispatch-blogs*)
           (let ((,uri% (concatenate-url (blog-url-root ,blog) ,uri)))
             (setf (blog-handler-alist ,blog)
                   (delete-if (lambda (,uri-handler)
                                (equal ,uri% (blog-uri-handler-uri ,uri-handler)))
                              (blog-handler-alist ,blog)))
             (push (make-instance
                    'blog-uri-handler
                    :uri ,uri%
                    :handler (lambda (&key ,@key-arg-list)
                               (funcall ,blog-fn ,blog ,@(mapcan (lambda (x y)
                                                                   (list x y))
                                                                 key-keywords
                                                                 key-args))))
                   (blog-handler-alist ,blog))))))))

#+nil
(defmacro define-authorized-blog-handler (description lambda-list blog-fn)
  `(define-blog-handler ,description ,lambda-list ,blog-fn))

(defmacro define-authorized-blog-handler (description lambda-list blog-fn)
  (when (atom description)
    (setq description (list description)))
  (destructuring-bind (blog
                       &key
                       uri
                       (default-parameter-type ''string)
                       (default-request-type :both))
      description
    (let* ((key-arg-list (loop for part in lambda-list
                            collect (hunchentoot::make-defun-parameter
                                     part
                                     default-parameter-type
                                     default-request-type)))
           (key-args (mapcar (lambda (arg)
                               (if (listp arg)
                                   (car arg)
                                   arg))
                             key-arg-list))
           (key-keywords (mapcar (lambda (arg)
                                   (make-keyword
                                    (symbol-name arg)))
                                 key-args)))
      (with-unique-names (uri-handler uri%)
        `(progn
           (pushnew ,blog *blog-dispatch-blogs*)
           (flet ((handler ,(cons blog '&key key-args)
                    (print (cons :user user) *debug-io*)
                    (print (cons :password password) *debug-io*)
                    (hunchentoot-auth:authorized-page
                     ((blog-realm blog) user password
                      :ssl-port (blog-ssl-port blog)
                      :login-page-function (lambda ()
                                             (blog-login-page blog user password)))
                     (funcall ,blog-fn blog ,@(mapcan (lambda (x y)
                                                        (list x y))
                                                      key-keywords
                                                      key-args)))))
             (let ((,uri% (concatenate-url (blog-url-root ,blog) ,uri)))
               (setf (blog-handler-alist ,blog)
                     (delete-if (lambda (,uri-handler)
                                  (equal ,uri% (blog-uri-handler-uri ,uri-handler)))
                                (blog-handler-alist ,blog)))
               (push (make-instance
                      'blog-uri-handler
                      :uri ,uri%
                      :handler 
                      (lambda (&key ,@key-arg-list)
                        (funcall (function handler) blog ,@(mapcan (lambda (x y)
                                                                     (list x y))
                                                                   key-keywords
                                                                   key-args))))
                     (blog-handler-alist ,blog)))))))))

(defmethod add-user ((blog blog) user password)
  (hunchentoot-auth:add-user (blog-realm blog) user password))

