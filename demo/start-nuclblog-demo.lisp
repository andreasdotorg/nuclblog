

(require 'asdf)
(asdf:oos 'asdf:load-op :nuclblog-demo)
(asdf:oos 'asdf:load-op :swank)
(handler-case
    (swank:create-server :dont-close t)
  (sb-bsd-sockets::socket-error))

(in-package :nuclblog-demo)

(multiple-value-bind (server ssl-server)
    (start-services)
  (defparameter *nuclblog-demo-server* server)
  (defparameter *nuclblog-demo-ssl-server* ssl-server))

