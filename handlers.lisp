
(in-package :nuclblog)

(defun entry (entry)
  (with-html
    (:div :class "entry"
          (:div :class "entry-head"
                (:div :class "entry-title"
                      (:h1 (str (blog-entry-title entry))))
                (:div :class "entry-date"
                      (:h2 (str (hunchentoot::rfc-1123-date
                                 (blog-entry-time entry)))))
                (let ((user (blog-entry-user entry)))
                  (when user
                    (htm (:div :class "entry-user"
                               (:h3 "posted by " (str user)
                                    " in " (str (blog-entry-category entry))))))))
          (:div :class "entry-contents"
                (str (blog-entry-contents entry))))))

(defun define-blog-handlers (blog)
  
  (define-easy-handler (blog-main :uri (blog-url-root blog))
      ()
    (blog::blog-page
     blog
     (blog-title blog)
     (lambda ()
       (loop for entry in (sorted-blog-entries blog)
            for i below 10
          do (entry entry)))))

  (define-easy-handler (blog-new :uri (concatenate-url (blog-url-root blog) "/new"))
      ()
    (blog::blog-page
     blog
     (format nil "~A: new entry" (blog-title blog))
     (lambda ()
       (with-html
         (:p "Sorry, adding new blog entries not supported yet.")))))

  (define-easy-handler (blog-archives :uri (concatenate-url (blog-url-root blog) "/archives"))
      (category)
    (blog::blog-page
     blog
     "Make me a BLOG!!"
     (lambda ()
       (loop for entry in (sorted-blog-entries blog)
          when (or (null category)
                   (equal (blog-entry-category entry)
                          category))
          do (entry entry)))))

  (define-easy-handler (blog-archives-rss :uri (concatenate-url (blog-url-root blog) "/archives.rss"))
      ()
    (blog::blog-page
     blog
     "Make me a BLOG!!"
     (lambda ()
       (with-html
         (:p "Sorry, blog entry RSS archives not supported yet.")))))

  (define-easy-handler (blog-email :uri (concatenate-url (blog-url-root blog) "/email"))
      ()
    (blog::blog-page
     blog
     "Make me a BLOG!!"
     (lambda ()
       (with-html
         (:p "Sorry, emailing the maintainer not supported yet.")))))

  (define-easy-handler (blog-display :uri (concatenate-url (blog-url-root blog) "/display"))
      ((id :parameter-type 'integer))
    (blog::blog-page
     blog
     "Make me a BLOG!!"
     (lambda ()
       (if (and id (numberp id))
           (entry
             (get-entry id blog))
           (with-html
             (:p "Please select a blog entry for display.")))))))

