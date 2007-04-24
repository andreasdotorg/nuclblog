
(in-package :nuclblog)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf who::*downcase-tags-p* nil))

(defun entry-html (blog entry)
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
  (with-html
    (:|item|
     (:|title| (str (blog-entry-title entry)))
     (:|link| (str (make-full-entry-url blog entry)))
     (:|description| (str (escape-string (blog-entry-contents entry))))
     (:|pubDate| (str (hunchentoot::rfc-1123-date
                      (blog-entry-time entry))))
     (:|guid| (str (make-full-entry-url blog entry))))))

(defun define-blog-handlers (blog)
  
  (define-easy-handler (blog-main :uri (blog-url-root blog))
      ()
    (blog::blog-page
     blog
     (blog-title blog)
     (lambda ()
       (loop for entry in (sorted-blog-entries blog)
          for i below 10
          do (entry-html blog entry)))))

  (define-easy-handler (blog-archives :uri (concatenate-url (blog-url-root blog) "/archives"))
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

  (define-easy-handler (blog-archives-rss :uri (concatenate-url (blog-url-root blog) "/archives.rss"))
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

  (define-easy-handler (blog-email :uri (concatenate-url (blog-url-root blog) "/email"))
      ()
    (redirect "mailto:webmaster@cyrusharmon.org" :permanently t))

  (define-easy-handler (blog-display :uri (concatenate-url (blog-url-root blog) "/display"))
      ((id :parameter-type 'integer))
    (blog::blog-page
     blog
     (format nil "~A: display" (blog-title blog))
     (lambda ()
       (if (and id (numberp id))
           (entry-html blog (get-entry id blog))
           (with-html
             (:p "Please select a blog entry for display."))))))

  (define-easy-handler (blog-new-browser-auth :uri (concatenate-url (blog-url-root blog) "/new-browser-auth")
                                 :default-request-type :post)
      (category content title)
    (multiple-value-bind (user password)
        (authorization)
      (if (validate-user user password)
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

  (define-easy-handler (blog-new :uri (concatenate-url (blog-url-root blog) "/new")
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
  
  (define-easy-handler (blog-edit :uri (concatenate-url (blog-url-root blog) "/edit")
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

  (define-easy-handler (blog-delete :uri (concatenate-url (blog-url-root blog) "/delete"))
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
  
  (define-easy-handler (blog-login :uri (concatenate-url (blog-url-root blog) "/login")
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

  (define-easy-handler (blog-logout :uri (concatenate-url (blog-url-root blog) "/logout"))
      ()
    (setf (session-value 'user-authenticated-p) nil)
    (blog::blog-page
     blog
     (format nil "~A: logout" (blog-title blog))
     (lambda ()
       (with-html
         (:p "You have successfully logged out."))))))

