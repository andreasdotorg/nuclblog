
(in-package :nuclblog)

(defparameter *use-editor* t)


(defun entry-link (blog entry)
  (concatenate-url
   (blog-display-entry-url blog)
   "?id="
   (url-encode (prin1-to-string (blog-entry-number entry)))))

(defun archives-url (blog &key category rss)
  (if category
      (concatenate 'string
                   (if rss
                       (blog-rss-url blog)
                       (blog-archives-url))
                   (if rss "?" "/")
                   category)
      (if rss (blog-rss-url blog) (blog-archives-url blog))))

(defmacro box ((&key class id) head &rest body)
  `(with-html-output (*standard-output*)
     (:div :id ,id :class ,(concatenate 'string "box " class)
           ,(when head
                  `(:div :class "box-head"
                         ,head))
           (:div :class "box-body"
                 ,@body))))

(defun recent-entries (blog)
  (with-html
    (box (:class "nav-box" :id "nav-box-recent-entries")
         (:h2 "Recent entries")
         (:ul :class "recent-entries"
              (loop for i from 1 to 10
                 for j in (blog-entries blog)
                 do (htm
                     (:li
                      (:a :href (entry-link blog j)
                          (str (blog-entry-title j))))))))))

(defun categories (blog)
  (with-html
    (box (:class "nav-box" :id "nav-box-categories")
         (:h2 "Categories")
         (:ul
          (loop for i in (blog-categories blog)
             do (htm
                 (:li (:a :href (make-archives-url blog i)
                          (str i))
                      " "
                      (:a :href (archives-url blog :category i :rss t)
                          "(RSS)"))))))))

(defun buttons (blog)
  (with-html
    (box (:class "nav-box nav-button" :id "nav-box-buttons")
         nil
         (:ul :class "buttons"
              (loop for button in (blog-buttons blog)
                 do
                 (destructuring-bind (&key href-url id img-url alt) button
                   (htm
                    (:li
                     (:a :href href-url :class "button"
                         (:img :id id
                               :src img-url
                               :alt alt))))))))))

(defun main-nav (blog)
  (box (:class "nav-box" :id "nav-box-1")
               (:h2 (str (blog-short-name blog)))
               (:ul
                (:li (:a :href (blog-url-root blog) "Main"))
                (:li (:a :href (blog-new-entry-url blog) "New entry"))
                (:li (:a :href (blog-archives-url blog) "Archives"))
                (:li (:a :href (archives-url blog :rss t) "Syndicate (RSS)"))
                (:li (:b (:a :href (blog-email-redirect-url blog) "Send Comments"))))))

(defgeneric nav-boxes (blog))

(defmethod nav-boxes ((blog blog))
  (main-nav blog)
  (recent-entries blog)
  (categories blog))

(defun nav (blog)
  (with-html-output (*standard-output*)
    (:div :id "nav" :class "nav"
          (nav-boxes blog)
          (buttons blog))))

(defun banner (blog)
  (with-html-output (*standard-output*)
    (:div :id "banner"                  
          (let ((url (blog-logo-img-url blog)))
            (when url
              (htm (:div :id "bannerleft"
                         (:img :class "titlelogo" :src url :alt ""))
                   "&nbsp;")))
          (:div :id "bannertext"
                (:h1 (str (blog-title blog)))
                (:h2 (str (blog-subtitle blog))))
          (:div :class "pad" "&nbsp;"))))

(defun blog-page (blog title body-function)
  (with-html-page
    (:html
     (:head (:title (str title))
            (loop for style in (blog-page-css blog)
               for primary = t then nil
               do
               (htm
                (:link :rel (if primary "stylesheet" "alternate stylesheet")
                       :title (car style) :type "text/css" :href (cdr style)))))
     (:body
      (banner blog)
      (:div :id "main"
            (nav blog)
            (:div :id "content"
                  (funcall body-function)))))))