(require :asdf)
(push "~/workspace/LABS/K3/translator/"
      asdf::*central-registry*)
(declaim (optimize (debug 3)))
(asdf:load-system :translator)
