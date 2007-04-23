
(in-package :nuclblog)

(defun make-entry-url (blog entry)
  (concatenate-url (blog-url-root blog)
                   "/display?id="
                   (princ-to-string (blog-entry-number entry))))

(defun make-archives-url (blog category)
  (format nil "~A/archives?category=~A" (blog-url-root blog) category))
