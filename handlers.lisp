
(in-package :nuclblog)

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
                (str (blog-entry-contents entry))))))

(defun entry-rss (blog entry)
  (with-html
    (:item
     (:title (str (blog-entry-title entry)))
     (:link (str (make-entry-url blog entry)))
     (:description (str (escape-string (blog-entry-contents entry))))
     (:pubdate (str (hunchentoot::rfc-1123-date
                      (blog-entry-time entry))))
     (:guid (str (make-entry-url blog entry))))))

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
      (htm (:rss :version 2.0
                 (:channel
                  (:title (str (blog::blog-title blog)))
                  (:link (str (blog::blog-url-root blog)))
                  (:description (str (blog::blog-subtitle blog)))
                  (:pubdate (str (hunchentoot::rfc-1123-date)))
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
             (:p "Please select a blog entry for display.")))))))

