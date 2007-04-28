

(require 'asdf)
(asdf:oos 'asdf:load-op :nuclblog-demo)
(asdf:oos 'asdf:load-op :swank)
(swank:create-server :dont-close t)

(in-package :nuclblog-demo)

(defparameter *nuclblog-demo-server* (start-services))
(defparameter *nuclblog-demo-ssl-server* (start-ssl-services))

