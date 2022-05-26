SHELL = /bin/sh
EMACS ?= emacs
PROFILER =
EMACS_BATCH_OPTS=--batch -Q -l find-file-in-project.el
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
