
(in-package :nuclblog)

(defun entry (entry)
  (with-html
    (:div :class "entry"
          (:div :class "entry-head"
                (:div :class "entry-title"
                      (:h1 (str (blog-entry-title entry))))
                (:div :class "entry-date"
                      (:h2 (str (format-universal-time
                                 (blog-entry-time entry))))))
          (:div :class "entry-contents"
                (str (blog-entry-contents entry))))))

(defun define-blog-handlers (blog)
  
  (define-easy-handler (blog-main :uri (blog-url-root blog))
      ()
    (blog::blog-page
     blog
     (blog-title blog)
     (lambda ()
       (loop for entry in (blog-entries blog)
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
      ()
    (blog::blog-page
     blog
     "Make me a BLOG!!"
     (lambda ()
       (with-html
         (:p "Sorry, blog entry archives not supported yet.")))))

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

