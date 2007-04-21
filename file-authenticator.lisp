
(in-package :nuclblog)

(defparameter *users* nil)
(defvar *passwords* nil)
(defvar *password-file-loaded* nil)

(defparameter *password-file*
  (merge-pathnames 
   (make-pathname :directory (list :relative :up)
		  :name "password")
   (make-pathname :directory
		  (pathname-directory
		   (or *load-truename*
		       *default-pathname-defaults*)))))

(defclass file-authenticator (authenticator) ())

(defmethod load-password-file ((a file-authenticator))
  (with-open-file (p *password-file* :direction :input :if-does-not-exist nil)
    (if p
        (prog1
	    (setf *passwords* (read p))
	  (setf *password-file-loaded* t))
        (auth-change-all-passwords a))))

(defmethod auth-change-all-passwords ((a file-authenticator))
  (loop for user in *users*
             do
             (progn
               (format t "Please enter the password that will be used to secure this blog for user ~A: " user)
               (finish-output)
               (auth-change-password-to a user (read-line))))
  (with-open-file (passfile *password-file* :direction :output :if-exists :supersede)
    (write *passwords* :stream passfile)))

(defmethod auth-change-password-to ((a file-authenticator) user new-password)
  (let ((u (assoc user *passwords* :test #'string-equal))
	(md5p (md5:md5sum-sequence new-password)))
    (if u
	(setf (cdr u) md5p)
	(push (cons user (md5:md5sum-sequence new-password))
	      *passwords*))))

(defmethod auth-get-password ((a file-authenticator) user)
  (unless *password-file-loaded* (load-password-file a))
  (let ((a (assoc user *passwords* :test #'string-equal)))
    (if a
	(cdr a)
	nil)))

(defmethod check-user ((a file-authenticator) user password)
  (let ((u (assoc user *passwords* :test #'string-equal))
	(md5p (md5:md5sum-sequence password)))
    (and u (equalp (cdr u) md5p))))

