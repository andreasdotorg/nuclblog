
(in-package :nuclblog)

(defmacro with-html-page (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     ,@body))

(defmacro with-html (&body body)
  `(with-html-output (*standard-output*)
     ,@body))

(defmacro with-xml-output-to-string ((&optional (stream *standard-output*))
                           &body body)
  `(let ((who::*downcase-tags-p* nil))
     (with-html-output-to-string (,stream)
       (princ "<?xml version='1.0'?>" ,stream)
       ,@body)))


(defun concatenate-url (base &rest strings)
  (apply #'concatenate 'string base strings))

