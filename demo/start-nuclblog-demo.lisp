
(require 'asdf)
(asdf:oos 'asdf:load-op :nuclblog-demo)
(asdf:oos 'asdf:load-op :swank)
(swank:create-server :dont-close t)
(nuclblog-demo:start-services)
