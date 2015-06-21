PACKAGE := auto-package-update
PACKAGE_FILE = $(PACKAGE).el

EMACS := emacs
CURL := curl --silent
SHELL := /bin/bash

define download-dependency
	@$(CURL) $(DEPENDENCY_URL) > $@
endef

DEPENDENCIES = \
	dash.el	\
	make-readme-markdown.el

.PHONY: test

package: *.el
	@ver=`grep -o "Version: .*" $(PACKAGE_FILE) | cut -c 10-`; \
	tar cjvf $(PACKAGE)-$$ver.tar.bz2 --mode 644 `git ls-files '*.el' | xargs`

clean:
	rm -rf $(PACKAGE)-*/ $(PACKAGE)-*.tar* *.elc
	rm -rf $(DEPENDENCIES)

test: $(DEPENDENCIES)
	${EMACS} -Q --batch -L .  -L ./tests \
		-l tests/$(PACKAGE)-test.el \
		--eval "(ert-run-tests-batch-and-exit '(not (tag interactive)))"

.git/hooks/pre-commit: hooks/pre-commit
	$(shell cp {,.git/}hooks/pre-commit)
git-hooks: .git/hooks/pre-commit

# Automatically generate README.md
README.md: $(PACKAGE_FILE) | make-readme-markdown.el
	$(EMACS) --script make-readme-markdown.el < $< >$@ 2>/dev/null

# Dependencies
dash.el: DEPENDENCY_URL = https://raw.githubusercontent.com/magnars/dash.el/master/dash.el
dash.el:
	$(download-dependency)

make-readme-markdown.el: DEPENDENCY_URL = https://raw.githubusercontent.com/mgalgs/make-readme-markdown/master/make-readme-markdown.el
make-readme-markdown.el:
	$(download-dependency)
