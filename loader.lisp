(require :asdf)
(push "~/workspace/LABS/K3/translator/"
      asdf::*central-registry*)
(declaim (optimize (speed 3) (safety 0) (debug 0)))
(asdf:load-system :translator)
