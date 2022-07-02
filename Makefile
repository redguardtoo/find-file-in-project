SHELL = /bin/sh
EMACS ?= emacs
PROFILER =
EMACS_GENERIC_OPTS=-Q -L . -L deps/ivy-0.13.4
EMACS_BATCH_OPTS:=--batch $(EMACS_GENERIC_OPTS)
RM = @rm -rf

.PHONY: test clean test compile

# Delete byte-compiled files etc.
clean:
	$(RM) *~
	$(RM) \#*\#
	$(RM) *.elc

compile: clean
	@$(EMACS) $(EMACS_BATCH_OPTS) -l tests/my-byte-compile.el 2>&1 | grep -E "([Ee]rror|[Ww]arning):" && exit 1 || exit 0

# Run tests.
test: compile
	@$(EMACS) $(EMACS_BATCH_OPTS) -l tests/ffip-tests.el

runemacs:
	@mkdir -p deps;
	@if [ ! -f deps/ivy-0.13.4/ivy.el ]; then curl -L https://stable.melpa.org/packages/ivy-0.13.4.tar | tar x -C deps/; fi;
	@$(EMACS) $(EMACS_GENERIC_OPTS) -l ./tests/emacs-init.el
