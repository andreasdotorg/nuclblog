
(in-package :nuclblog)

(defmacro with-html-page (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     ,@body))

(defmacro with-html (&body body)
  `(with-html-output (*standard-output*)
     ,@body))

(defun concatenate-url (base &rest strings)
  (apply #'concatenate 'string base strings))

(defparameter *date-formatter*
  (formatter "~3/net.telent.date:dayname/, ~4,'0D ~3/net.telent.date:monthname/ ~D ~2,'0D:~2,'0D:~2,'0D"))

(defun format-date (day year month date hour minute second)
  (format nil
        *date-formatter*
        day year month date hour minute second))

(defun format-universal-time (universal-time)
  (multiple-value-bind (second minute hour date month year day)
      (decode-universal-time universal-time)
    (format-date day year month date hour minute second)))
