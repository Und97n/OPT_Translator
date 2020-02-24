PROJECT_LOCATION := ~/workspace/LABS/K3/translator/
SBCL_BINARY := /usr/bin/sbcl
OUT_BINARY := translator
CACHE :=~/.cache/common-lisp
TESTS := $(shell find $(PROJECT_LOCATION) -name "*.test" -printf "\\\"%p\\\" ")

rebuild: clean build

clean:
	-@rm $(OUT_BINARY)
	-@rm -rf $(CACHE)

build:
	@echo "(require :asdf) \
	       (push \"$(PROJECT_LOCATION)\" asdf:*central-registry*) \
	       (asdf:load-system :translator) \
	       (save-lisp-and-die \"$(OUT_BINARY)\" :executable t)" | $(SBCL_BINARY)
	@chmod +x $(OUT_BINARY)

testing:
	@echo "(require :asdf) \
	       (push \"$(PROJECT_LOCATION)\" asdf:*central-registry*) \
	       (asdf:load-system :translator) \
	       (asdf:load-system :translator/lexer/tests) \
	       (translator/lexer/tests:run-tests $(TESTS)) \
	       (sb-ext:exit)" | $(SBCL_BINARY)

testlist:
	@echo "$(TESTS)"
