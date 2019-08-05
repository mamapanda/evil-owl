emacs ?= emacs
bemacs ?= $(emacs) --batch -l test/elpa.el

all: compile

checkdoc:
	$(bemacs) -l test/make-checkdoc.el

compile:
	$(bemacs) -l test/make-compile.el

clean:
	rm -f *.elc

package-lint:
	$(bemacs) -f package-lint-batch-and-exit evil-owl.el

test:
	$(bemacs) -l test/make-test.el

update:
	$(emacs) --batch -l test/make-update.el

.PHONY: all checkdoc clean compile package-lint test update
