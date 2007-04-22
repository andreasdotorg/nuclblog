
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
		  :initform (make-instance 'file-authenticator))
   (allow-html-editor :initarg allow-html-editor :initform nil
		      :accessor blog-allow-html-editor)
   (url-root :initarg :url-root :accessor blog-url-root)
   (entries :accessor blog-entries :initform (list))
   (buttons :initarg :buttons :accessor blog-buttons :initform (list))
   (entry-storage-path :initarg :entry-storage-path
                       :accessor blog-entry-storage-path :initform nil)))

(defclass blog-entry ()
  ((category :initarg :category :accessor blog-entry-category)
   (user :initarg :user :accessor blog-entry-user)
   (number :initarg :number :accessor blog-entry-number)
   (title :initarg :title :accessor blog-entry-title)
   (time :initarg :time :accessor blog-entry-time)
   (revised-time :initarg :revised-time :initform 0 :accessor blog-entry-revised-time)
   (contents :initarg :contents :accessor blog-entry-contents)
   (trackbacks :initarg :trackbacks :accessor blog-entry-trackbacks :initform nil)))

(defmethod blog-new-entry-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/new"))

(defmethod blog-display-entry-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/display"))

(defmethod blog-archives-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/archives"))

(defmethod blog-rss-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/archives.rss"))

(defmethod blog-edit-entry-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/edit"))

(defmethod blog-delete-entry-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/delete"))

(defmethod blog-email-redirect-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/email"))

(defmethod blog-trackback-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/trackback"))

(defmethod blog-new-category-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/new-category"))

(defmethod blog-css-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/css"))

(defun get-entry (number blog)
  (find number (blog-entries blog) :key #'blog-entry-number))

