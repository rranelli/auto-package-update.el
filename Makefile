EMACS := emacs
PACKAGE := auto-package-update

.PHONY: test

package: *.el
	@ver=`grep -o "Version: .*" $(PACKAGE).el | cut -c 10-`; \
	tar cjvf $(PACKAGE)-$$ver.tar.bz2 --mode 644 `git ls-files '*.el' | xargs`

clean:
	@rm -rf $(PACKAGE)-*/ $(PACKAGE)-*.tar* *.elc

test:
	${EMACS} -Q --batch -L .  -L ./tests \
		-l tests/$(PACKAGE)-test.el \
		--eval "(ert-run-tests-batch-and-exit '(not (tag interactive)))"
