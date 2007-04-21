
(in-package :nuclblog)

(defparameter *use-editor* t)


(defun entry-link (blog entry)
  (puri:merge-uris
   (blog-display-entry-url blog)
   (prin1-to-string (blog-entry-number entry))))

(defun archives-url (blog &key category rss)
  (if category
      (concatenate 'string
                   (puri:render-uri
                    (if rss
                        (blog-rss-url blog)
                        (blog-archives-url))
                    nil)
                   (if rss "?" "/")
                   category)
      (puri:render-uri (if rss (blog-rss-url blog) (blog-archives-url blog)) nil)))

(defun make-archives-url (blog &key category month year)
  (let ((url (puri:copy-uri (blog-archives-url blog))))
    (setf (puri:uri-query url) (format nil "~A~A~A"
                                  (if category
                                      (format nil "category=~A" category)
                                      "")
                                  (if (and category month year)
                                      "&"
                                      "")
                                  (if (and month year)
                                      (format nil "month=~A&year=~A"
                                              month year)
                                      "")))
    (html-encode:encode-for-pre (puri:render-uri url nil))))

(defun lsidebar (blog)
  (with-html-output (*standard-output*)
    (:div :id "lsidebar" :class "sidebar"
          (:div :id "lsidebar-inset" :class "sidebar-inset"
                (:h2 (blog-short-name blog))
               
                (:ul
          
                 (:li (:a :href (puri:render-uri (blog-url-root blog) nil) "Main"))
                 (:li (:a :href (puri:render-uri (blog-new-entry-url blog) nil) "New entry"))
                 (:li (:a :href (puri:render-uri (blog-archives-url blog) nil) "Archives"))
                 (:li (:a :href (archives-url blog :rss t) "Syndicate (RSS)"))
                 (:li (:b (:a :href (puri:render-uri (blog-email-redirect-url blog) nil) "Send Comments"))))
                (:h2 "Recent entries")
                (:ul
                 (loop for i from 1 to 10
                    for j in (blog-entries blog)
                    do (htm
                        (:li
                         (:a :href (puri:render-uri
                                    (entry-link blog j))
                             (blog-entry-title j))))))
                (:h2 "Categories")
                (:ul
                 (loop for i in (blog-categories blog)
                    do (htm
                        (:li (:a :href (make-archives-url blog :category i)
                                 i) " "
                                 (:a :href (archives-url blog :category i :rss t)
                                     "(RSS)")))))
                #+sbcl
                (progn
                  (htm
                   (:p :align "center")
                   (:a :href "http://www.sbcl.org/" :style "border: none"
                       (:img :id "sbclbutton"
                             :src "http://www.sbcl.org/sbclbutton.png"
                             :alt "(get 'sbcl)"
                             :style "border: none"))))))))

(defun rsidebar (blog)
  (with-html-output (*standard-output*)
    (:div :id "rsidebar" :class "sidebar"
          (:div :id "rsidebar-inset" :class "sidebar-inset"
                (:h2 "Blogs")
                (:ul
                 (mapcar (lambda (blog)
                           (htm (:li (:a :href (car blog) :target "_blank" (cdr blog)))))
                         (blog-blog-links blog)))
                (:h2 "People")
                (:ul
                 (mapcar (lambda (person)
                           (htm (:li (:a :href (car person) :target "_blank" (cdr person)))))
                         (blog-people-links blog)))))))

(defun blog-page (blog title body-function)
  (with-html-page
    (:html
     (:head (:title title)
            (loop for style in (blog-page-css blog)
               for primary = t then nil
               do
               (htm
                (:link :rel (if primary "stylesheet" "alternate stylesheet")
                       :title (car style) :type "text/css" :href (cdr style))))
            #+nil (if *use-editor*
                      (htm (:script :type "text/javascript"
                                    "_editor_url = \"/htmlarea/\";
_editor_lang = \"en\";")
                           (:script :type "text/javascript" :src "/htmlarea/htmlarea.js")
                           (:script :type "text/javascript" :src "/htmlarea/dialog.js")
                           (:script :type "text/javascript" :src "/htmlarea/lang/en.js"))))
     (:body #+nil (if *use-editor* :onload)
            #+nil (if *use-editor* "HTMLArea.replaceAll();")

            (:div :id "inset"
                  (:div :id "banner"                  
                        (:div :id "bannerleft"
                              (:img :class "titlelogo" :src "/images/blog-logo.png" :alt "")
                              "&nbsp;")
                        (:div :id "bannertext"
                              (:h1 (str title))
                              (:h2 (str (blog-subtitle blog))))

                        (:div :class "pad" "&nbsp;"))

                  (lsidebar blog)
                  
                  (:div :id "contentcol"
                        (:div :id "content"
                              (funcall body-function))
                        (rsidebar blog))
                  (:div :class "pad" "&nbsp;"))))))