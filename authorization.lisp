
(in-package :nuclblog)

(defun validate-user (user passwd)
  (and (string= user "cyrus")
       (string= passwd "bogus")))

(defmacro authorized-page ((user password) &rest body)
  `(if (or (and ,user ,password)
           (validate-user ,user password)
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
                  (:input :type :submit :value "Submit")))))))