
(in-package :nuclblog)

(defun get-protocol ()
  (symbol-name (server-protocol)))

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

(defun make-archives-url (blog category)
  (format nil "~A/archives?category=~A" (blog-url-root blog)
          (url-encode category)))

(defmethod blog-new-entry-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/new"))

(defmethod blog-login-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/login"))

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

