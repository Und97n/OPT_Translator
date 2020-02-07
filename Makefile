PROJECT_LOCATION="~/workspace/LABS/K3/translator/"
SBCL_BINARY="/usr/bin/sbcl"

build:
	echo "(require :asdf) \
	      (push $(PROJECT_LOCATION) asdf:*central-registry*) \
	      (asdf:load-system :translator) \
	      (sb-ext:exit)" | $(SBCL_BINARY)
