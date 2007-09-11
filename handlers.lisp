;;; file: handlers.lisp
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

(defun entry-html (blog entry)
  "Outputs html for a blog entry."
  (with-html
    (:div :class "entry"
          (:div :class "entry-head"
                (:div :class "entry-title"
                      (:h1 (:a :href (make-entry-url blog entry)
                               (str (blog-entry-title entry)))))
                (:div :class "entry-date"
                      (:h2 (str (hunchentoot::rfc-1123-date
                                 (blog-entry-time entry)))
                           (unless (< (abs (- (blog-entry-time entry)
                                              (blog-entry-revised-time entry)))
                                      10)
                             (htm
                              " revised at: "
                              (str (hunchentoot::rfc-1123-date
                                    (blog-entry-revised-time entry)))))))
                
                (let ((user (blog-entry-user entry)))
                  (when user
                    (htm (:div :class "entry-user"
                               (:h3 "posted by " (str user)
                                    " in " (:a :href
                                               (make-archives-url
                                                blog (blog-entry-category entry))
                                               (str (blog-entry-category entry)))))))))
          (:div :class "entry-contents"
                (str (blog-entry-contents entry)))
          (:div :class "entry-nav"
                (when (hunchentoot-auth:session-user-authenticated-p)
                  (htm (:a :href (make-edit-entry-url blog entry) "edit")
                       " "
                       (:a :href (make-delete-entry-url blog entry) "delete")))))))

;;; ugh. who::*downcase-tags-p* needs to be set at compile time. Let's
;;; try to be polite about how we got about setting this global
;;; flag. I wish there a better way to do this...

(defparameter *tag-state* who::*downcase-tags-p*)

(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *tag-state* who::*downcase-tags-p*)
    (setf who::*downcase-tags-p* nil))  

  (defun entry-rss (blog entry)
    "Outputs RSS 2.0 for a given blog entry."
    (with-xml
      (:|item|
        (:|title| (str (blog-entry-title entry)))
        (:|link| (str (make-full-entry-url blog entry)))
        (:|description| (str (escape-string (blog-entry-contents entry))))
        (:|pubDate| (str (hunchentoot::rfc-1123-date
                          (blog-entry-time entry))))
        (:|guid| (str (make-full-entry-url blog entry))))))

  (defun channel-rss (blog &key (limit 10) category)
    (setf (content-type) "application/rss+xml")
    (with-xml-output-to-string (*standard-output*)
      (htm (:|rss| :|version| 2.0
             (:|channel|
               (:|title| (str (blog-title blog)))
               (:|link| (str (make-full-root-url blog)))
               (:|description| (str (blog-subtitle blog)))
               (:|pubDate| (str (hunchentoot::rfc-1123-date)))
               (loop for entry in
                    (apply #'sorted-blog-entries blog
                           (when category
                             `(:category ,category)))
                    for i below limit
                    do (entry-rss blog entry)))))))
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (setf who::*downcase-tags-p* *tag-state*)))

(defun blog-login-page (blog &optional user password)
  (blog-page
   blog
   (format nil "~A: login" (blog-title blog))
   (lambda ()
     (if (or user password)
         (with-html
           (:p "Login failed. Please try again."))
         (with-html
           (:p "Please login:")))
     (hunchentoot-auth:generate-html-login :user user :password password))))

;;; this only exists for the (deprecated, test) new-browser-auth
;;; handler. this should go away, quickly...
(defmethod check-password ((blog blog) user password)
  (hunchentoot-auth:check-password (blog-realm blog) user password))

(defun define-blog-handlers (blog)
  "Defines the easy handlers for a given blog."

  (define-blog-handler (blog)
      ()
    (blog-page
     blog
     (blog-title blog)
     (lambda ()
       (loop for entry in (sorted-blog-entries blog)
          for i below 10
          do (entry-html blog entry)))))
  
  (define-blog-handler (blog :uri "/archives")
      (category)
    (blog-page
     blog
     (format nil "~A: archives" (blog-title blog))
     (lambda ()
       (loop for entry in (sorted-blog-entries blog)
          when (or (null category)
                   (equal (blog-entry-category entry)
                          category))
          do (entry-html blog entry)))))

  (define-blog-handler (blog :uri "/archives.rss")
      ((limit :parameter-type 'integer :init-form 10)
       category)
    (apply #'channel-rss blog :limit limit
           (when category
             `(:category ,category))))

  (define-blog-handler (blog :uri "/email")
      ()
    (redirect (format nil "mailto:~A" (blog-owner-email blog)) :permanently t))

  (define-blog-handler (blog :uri "/display")
      ((id :parameter-type 'integer))
    (blog-page
     blog
     (format nil "~A: display" (blog-title blog))
     (lambda ()
       (if (and id (numberp id))
           (entry-html blog (get-entry id blog))
           (with-html
             (:p "Please select a blog entry for display."))))))

  (define-blog-handler (blog :uri "/new-browser-auth"
                             :default-request-type :post)
      (category content title)
    (multiple-value-bind (user password)
        (authorization)
      (if (check-password blog user password)
          (blog-page
           blog
           (format nil "~A: new entry" (blog-title blog))
           (lambda ()
             (if (and content title category)
                 (progn
                   (create-blog-entry blog category title content user)
                   (with-html
                     (:p (str (format nil "Created new blog entry by ~A in ~A:"
                                      user category)))
                     (:h2 (str title))
                     (:p (str content))))
                 (with-html
                   (:p (:form :method :post
                              "Category: "
                              (:select :name "category"
                                       (loop for cat in (blog-categories blog)
                                          for selected = t then nil
                                          do (if selected
                                                 (htm (:option :selected t :label cat (str cat)))
                                                 (htm (:option :label cat (str cat))))))
                              (:br)
                              "Title: "
                              (:input :type :text :name "title" (when title (str title)))
                              (:br)
                              (:textarea :name "content" :rows "20" :cols "60" "")
                              (:br)
                              (:input :type :submit :value "Submit" (when content (str content)))))))))
          (require-authorization))))

  (define-blog-handler (blog :uri "/new"
                             :default-request-type :post)
      (category
       content
       title
       (user :init-form (hunchentoot-auth:session-user))
       (password))
    (hunchentoot-auth:authorized-page
     ((blog-realm blog) user password
      :login-page-function (lambda ()
                             (blog-login-page blog user password)))
     (blog-page
      blog
      (format nil "~A: new entry" (blog-title blog))
      (lambda ()
        (if (and content title category)
            (progn
              (create-blog-entry blog category title content user)
              (with-html
                (:p (str (format nil "Created new blog entry by ~A in ~A:"
                                 user category)))
                (:h2 (str title))
                (:p (str content))))
            (with-html
              (:p (:form :method :post
                         "Category: "
                         (:select :name "category"
                                  (loop for cat in (blog-categories blog)
                                     for selected = t then nil
                                     do (if selected
                                            (htm (:option :selected t :label cat (str cat)))
                                            (htm (:option :label cat (str cat))))))
                         (:br)
                         "Title: "
                         (:input :type :text :name "title" (when title (str title)))
                         (:br)
                         (:textarea :name "content" :rows "20" :cols "60" "")
                         (:br)
                         (:input :type :submit :value "Submit")))))))))
  
  (define-blog-handler (blog :uri "/edit"
                             :default-request-type :both)
      ((id :parameter-type 'integer)
       category
       content
       title
       (user :init-form (hunchentoot-auth:session-user))
       (password))
          
    (hunchentoot-auth:authorized-page
     ((blog-realm blog) user password
      :login-page-function (lambda ()
                             (blog-login-page blog user password)))
     (let ((edited)
           (edit-error))
       (when (and id content title category)
         (let ((entry (get-entry id blog)))
           (if entry
               (progn
                 (setf (blog-entry-category entry) category)
                 (setf (blog-entry-title entry) title)
                 (setf (blog-entry-contents entry) content)
                 (setf (blog-entry-revised-time entry) (get-universal-time))
                 (let ((path (blog-entry-storage-path blog)))
                   (when path (store-blog-entries blog path)))
                 (setf edited t))
               (setf edit-error t))))
       (blog-page
        blog
        (format nil "~A: edit entry" (blog-title blog))
        (lambda ()
          (cond (edited
                 (with-html
                   (:p (str (format nil "updated new blog entry by ~A in ~A:"
                                    user category)))
                   (:h2 (str title))
                   (:p (str content))))
                (edit-error
                 (with-html
                   (:p "Entry editing error!")))
                (t
                 (let* ((entry (get-entry id blog)))
                   (when entry
                     (let ((category (or category (blog-entry-category entry))))
                       (with-html
                         (:p (:form :method :post
                                    "Category: "
                                    (:select :name "category"
                                             (loop for cat in (blog-categories blog)
                                                do (if (equal cat category)
                                                       (htm (:option :selected t :label cat (str cat)))
                                                       (htm (:option :label cat (str cat))))))
                                    (:br)
                                    "Title: "
                                    (:input :type :text :name "title" :value (blog-entry-title entry))
                                    (:input :type :hidden :name "id" :value (princ-to-string (blog-entry-number entry)))
                                    (:br)
                                    (:textarea :name "content" :rows "20" :cols "60"
                                               (str (blog-entry-contents entry)))
                                    (:br)
                                    (:input :type :submit :value "Submit"))))))))))))))

  (define-blog-handler (blog :uri "/delete")
      ((id :parameter-type 'integer)
       (user :init-form (hunchentoot-auth:session-user))
       (password))
    (hunchentoot-auth:authorized-page
     ((blog-realm blog) user password
      :login-page-function (lambda ()
                             (blog-login-page blog user password)))
     (let ((deleted))
       (when id
         (setf deleted (delete-blog-entry blog id)))
       (blog-page
        blog
        (if (and id deleted)
            (format nil "~A: delete entry" (blog-title blog))
            (format nil "~A: error" (blog-title blog)))
        (lambda ()
          (if (and id deleted)
              (with-html
                (:p "Deleted entry " (str (princ-to-string id))))
              (with-html
                (:p "Error deleting entry"))))))))
  
  (define-blog-handler (blog :uri "/login"
                             :default-request-type :post)
      ((user :init-form (hunchentoot-auth:session-user))
       (password))
    (hunchentoot-auth:authorized-page
     ((blog-realm blog) user password
      :login-page-function (lambda ()
                             (blog-login-page blog user password)))
     (blog-page
      blog
      (format nil "~A: login" (blog-title blog))
      (lambda ()
        (with-html
          (:p "User " (str user) " successfully logged in."))))))

  (define-blog-handler (blog :uri "/logout")
      ()
    (setf (hunchentoot-auth:session-user-authenticated-p) nil)
    (setf (hunchentoot-auth:session-user) nil)
    (blog-page
     blog
     (format nil "~A: logout" (blog-title blog))
     (lambda ()
       (with-html
         (:p "You have successfully logged out."))))))
