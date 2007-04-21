
(in-package :nuclblog)

(defmacro with-html-page (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     ,@body))

(defmacro with-html (&body body)
  `(with-html-output (*standard-output*)
     ,@body))

