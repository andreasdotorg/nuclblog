;;; file: utilities.lisp
;;;
;;; Copyright (c) 2007 Cyrus Harmon (ch-lisp@bobobeach.com)
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(in-package :nuclblog)

(defmacro with-html-page (&body body)
  "Executes BODY inside a cl-who:wtih-html-output-to-string body,
directing the output to *standard-output* and setting :prologue to t."
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     ,@body))

(defmacro with-html (&body body)
  "Executes BODY inside a cl-who:with-html-output body."
  `(with-html-output (*standard-output*)
     ,@body))

(defmacro with-xml-output-to-string ((&optional (stream *standard-output*))
                                     &body body)
  "Prints the <?xml...?> header to stream and Executes BODY inside of
a cl-who:with-html-output-to-string block. There is some machinery to
turn off downcasing of the tags, but I think this doesn't work as I
intended and should be removed."
  (let ((who::*downcase-tokens-p* nil))
    `(with-html-output-to-string (,stream)
       (princ "<?xml version='1.0'?>" ,stream)
       ,@body)))

(defmacro with-xml (&body body)
  "Executes BODY inside a cl-who:with-html-output body. This is a
synonym for with-html, but it would be nice if this could
automatically deal with turning off the tag downcasing, which
it currently doesn't."
  `(with-html-output (*standard-output*)
     ,@body))

(defun concatenate-url (base &rest strings)
  "Concatenates strings. In theory, this could be smarter about
checking validity of URLs, fixing redundant slashes, etc..."
  (apply #'concatenate 'string base strings))

