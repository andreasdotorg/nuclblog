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

;;; we need to not downcase tags because RSS is case-sensitive. In
;;; particular, the pubDate tag must be in camelCase. Yuck. I'm not
;;; sure we need the eval-when stuff, but I was having trouble getting
;;; this to work without it.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf who::*downcase-tags-p* nil))

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
                                 (blog-entry-time entry)))))
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
                (when (session-value 'user-authenticated-p)
                  (htm (:a :href (make-edit-entry-url blog entry) "edit")
                       " "
                       (:a :href (make-delete-entry-url blog entry) "delete")))))))

(defun entry-rss (blog entry)
  "Outputs RSS 2.0 for a given blog entry."
  (with-html
    (:|item|
     (:|title| (str (blog-entry-title entry)))
     (:|link| (str (make-full-entry-url blog entry)))
     (:|description| (str (escape-string (blog-entry-contents entry))))
     (:|pubDate| (str (hunchentoot::rfc-1123-date
                      (blog-entry-time entry))))
     (:|guid| (str (make-full-entry-url blog entry))))))

(defun define-blog-handlers (blog)
  "Defines the easy handlers for a given blog."
  
  (define-blog-handler (blog)
      ()
    (blog::blog-page
     blog
     (blog-title blog)
     (lambda ()
       (loop for entry in (sorted-blog-entries blog)
          for i below 10
          do (entry-html blog entry)))))
  
  (define-blog-handler (blog :uri "/archives")
      (category)
    (blog::blog-page
     blog
     (format nil "~A: archives" (blog-title blog))
     (lambda ()
       (loop for entry in (sorted-blog-entries blog)
          when (or (null category)
                   (equal (blog-entry-category entry)
                          category))
          do (entry-html blog entry)))))

  (define-blog-handler (blog :uri "/archives.rss")
      ((limit :parameter-type 'integer :init-form 10))
    (setf (content-type) "application/rss+xml")
    
    (with-xml-output-to-string (*standard-output*)
      (htm (:|rss| :|version| 2.0
             (:|channel|
               (:|title| (str (blog::blog-title blog)))
               (:|link| (str (make-full-root-url blog)))
               (:|description| (str (blog::blog-subtitle blog)))
               (:|pubDate| (str (hunchentoot::rfc-1123-date)))
               (loop for entry in (sorted-blog-entries blog)
                  for i below limit
                  do (entry-rss blog entry)))))))

  (define-blog-handler (blog :uri "/email")
      ()
    (redirect (format nil "mailto:~A" (blog-owner-email blog)) :permanently t))

  (define-blog-handler (blog :uri "/display")
      ((id :parameter-type 'integer))
    (blog::blog-page
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
          (blog::blog-page
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
       (user :init-form (session-value 'user))
       (password))
    (authorized-page
     (user password)
     (blog::blog-page
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
       (user :init-form (session-value 'user))
       (password))
          
    (authorized-page
     (user password)
     (let ((edited)
           (edit-error))
       (when (and id content title category)
         (let ((entry (get-entry id blog)))
           (if entry
               (progn
                 (setf (blog-entry-category entry) category)
                 (setf (blog-entry-title entry) title)
                 (setf (blog-entry-contents entry) content)
                 (setf edited t))
               (setf edit-error t))))
       (blog::blog-page
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
                 (let ((entry (get-entry id blog)))
                   (when entry
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
                                  (:input :type :submit :value "Submit")))))))))))))

  (define-blog-handler (blog :uri "/delete")
      ((id :parameter-type 'integer)
       (user :init-form (session-value 'user))
       (password))
    (authorized-page
     (user password)
     (let ((deleted))
       (when id
         (setf deleted (delete-blog-entry blog id)))
       (blog::blog-page
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
      ((user :init-form (session-value 'user))
       (password))
    (authorized-page
     (user password)
     (blog::blog-page
      blog
      (format nil "~A: login" (blog-title blog))
      (lambda ()
        (with-html
          (:p "User " (str user) " successfully logged in."))))))

  (define-blog-handler (blog :uri "/logout")
      ()
    (setf (session-value 'user-authenticated-p) nil)
    (blog::blog-page
     blog
     (format nil "~A: logout" (blog-title blog))
     (lambda ()
       (with-html
         (:p "You have successfully logged out."))))))
